# Aquarius ---------------------------------------------------------------------
# Move code for performing and handling responses out of each function
# Automatically set class of request and response based on "operation"

aq_get_url <- make_get_env("AQUARIUS_URL")
aq_get_user <- make_get_env("AQUARIUS_USER")
aq_get_pw <- make_get_env("AQUARIUS_PW")

aqts <- function(
  api = c("publish", "acquisition", "provisioning"),
  route = NULL
) {
  api <- match.arg(api)
  url <- aq_get_url()
  api <- switch(
    api,
    publish = "publish/v2",
    acquisition = "acquisition/v2",
    provisioning = "provisioning/v1"
  )
  verbose <- getOption("raquarius.verbose")
  request <- httr2::request(url) |>
    httr2::req_url_path_append(api) |>
    httr2::req_error(body = error_response) |>
    httr2::req_user_agent("https://github.com/jasonelaw/raquarius") |>
    httr2::req_headers(
      `Accept-Encoding` = "gzip"
    )
  if (verbose) {
    request <- httr2::req_verbose(request)
  }
  if (!is.null(route)) {
    request <- httr2::req_url_path_append(request, route)
  }
  request
}

#' Wrap AQUARIUS Time Series API
#'
#' This function wrap the api. Query parameters are passed via `...`. Standard R
#' recycling rules apply to query parameters. When arguments are recycled,
#' multiple requests to the API are made. The only tricky thing here is that
#' some API endpoints handle vector valued query parameters. In the url, these
#' are often represented by repeated arguments. You can specify these arguments
#' by passing in a list of atomic vectors: arg1 = c(1,2,3), arg2 = list(1:2,
#' 1:3, 1). This will result in three separate requests with query parameters:
#' ?arg1=1&arg2=1&arg2=2; ?arg1=2&arg2=1&arg2=2&arg2=3; and ?arg1=3&arg2=1. In
#' some cases, not wrapping in list will result in weird recycling and you won't
#' get what you intended (e.g., the previous example wouldn't work right without
#' the list()). In simple cases (e.g., arg1 = 1, arg2 = 1:3), the result will be
#' the same; however, the number of requests to the API will differ.
#'
#' If you're ever uncertain what requests are actually being performed and how
#' many for a set of query arguments, you can specify .perform = FALSE and the
#' the httr2::httr2_request objects will be returned.
aquarius <- function(
  ...,
  operation = NULL,
  api = c("publish", "acquisition", "provisioning"),
  .names = NULL
) {
  api <- match.arg(api)
  request <- aqts(api, operation)
  params <- handle_params(...)
  nms <- param_names(params)
  if (nrow(params) >= 1) {
    request <- pmap(
      params,
      \(...) {
        httr2::req_url_query(request, ..., .multi = "explode")
      }
    )
    names(request) <- nms
  } else {
    request <- list(request)
  }

  new_aqts_request(request, operation = operation, api = api)
}

perform_aqts_request <- function(
  x,
  method = c("sequential", "parallel"),
  on_error = "stop"
) {
  method <- match.arg(method)
  token <- get_auth_token()
  x <- req_auth_token(x, token)
  if (identical(method, "sequential")) {
    response <- httr2::req_perform_sequential(
      as.list(x),
      on_error = on_error
    )
  } else if (identical(method, "parallel")) {
    response <- httr2::req_perform_parallel(
      as.list(x),
      on_error = on_error
    )
  }
  names(response) <- names(x)
  new_aqts_response(
    response,
    attr(x, "api"),
    attr(x, "operation")
  )
}

error_response <- function(x) {
  body <- rlang::catch_cnd(resp_body_json(x))
  if (inherits(body, "error")) {
    return(NULL)
  }
  if (hasName(body, "ResponseStatus")) {
    body$ResponseStatus$Message
  } else {
    NULL
  }
}

get_auth_token <- function() {
  token_exists <- exists_token()
  if (token_exists) {
    token <- get_token()
    is_expired <- token_has_expired(token)
  }
  if (!token_exists || is_expired) {
    pubkey <- GetSessionPublickey()
    token <- PostSession(pubkey) # By default gets creds from env vars
    cache_token(token)
  }
  token
}

#' @export
req_auth_token <- function(x, token, ...) {
  UseMethod("req_auth_token")
}

#' @export
req_auth_token.httr2_request <- function(req, token = get_auth_token(), ...) {
  stopifnot(inherits(req, "httr2_request"))
  req <- req |>
    httr2::req_headers_redacted(
      `X-Authentication-Token` = as.character(token)
    )
  req
}

map_httr2_method <- function(x, f) {
  map(as.list(x), f) |>
    vctrs::vec_restore(x)
}

#' @export
req_auth_token.aqts_request <- function(req, token = get_auth_token(), ...) {
  map(as.list(req), \(x) req_auth_token(x, token = token)) |>
    vctrs::vec_restore(req)
}

req_perform_aqts <- function(req) {
  if (inherits(req, "aqts_request")) {
    response <- httr2::req_perform(req)
    cls <- setdiff(class(req), c("aqts_request", "httr2_request"))
    ret <- new_aqts_response(response, class = cls)
  } else if (inherits(req, "list") && length(req) > 1) {
    response <- httr2::req_perform_parallel(
      req,
      progress = TRUE,
      max_active = 10
    )
    cls <- map(req, \(x) setdiff(class(x), c("aqts_request", "httr2_request")))
    ret <- map2(response, cls, \(x, y) new_aqts_response(x, y))
  }
  ret
}

resp_body_aqts <- function(resp, query = NULL, max_simplify_lvl = 0L) {
  ret <- RcppSimdJson::fparse(
    json = resp_body_raw(resp),
    query = query,
    max_simplify_lvl = max_simplify_lvl
  )
  ret[c("Summary", "ResponseTime", "ResponseVersion")] <- NULL
  ret
}

aqts_resp_inform <- function(resp) {
  cli::cli_warn(
    c(
      "Summary:",
      "!" = resp$Summary,
      "i" = "ResponseTime: {resp$ResponseTime}",
      "i" = "ResponseVersion: {resp$ResponseVersion}"
    ),
    .frequency = "once",
    .frequency_id = "aqts-resp"
  )
}

aqts_perform_parallel <- function(x, fun, format = TRUE) {
  create_req <- function(x) rlang:::inject(fun(!!!x, .perform = FALSE))
  reqs <- x |>
    dplyr::mutate(.req_id = dplyr::row_number()) |>
    tidyr::nest(.by = .req_id, .key = "request_data") |>
    dplyr::mutate(
      requests = purrr::map(request_data, create_req),
      responses = httr2::req_perform_parallel(requests),
      responses = purrr::map(responses, new_aqts_response)
    )
  if (format) {
    reqs <- reqs |>
      dplyr::mutate(responses = map(responses, format_response)) |>
      dplyr::select(.req_id, responses) |>
      tidyr::unnest(responses)
  }
  reqs
}

handle_params <- function(...) {
  params <- rlang::list2(...)
  tibble::tibble(!!!params)
}

param_names <- function(x) {
  if (nrow(x) > 0) {
    ret <- imap(x, \(x, idx) paste0(idx, "=", x))
    pmap(ret, \(...) paste(..., sep = "--"))
  } else {
    character(0)
  }
}

handle_request <- function(request, .perform, .format, .parallel = FALSE, ...) {
  if (.perform) {
    ret <- perform_aqts_request(
      request,
      if (.parallel) "parallel" else "sequential"
    )
    if (.format) {
      ret <- format_response(ret, ...)
    }
  } else {
    ret <- request
  }
  ret
}

# AQTS request objects ---------------------------
#' @exportS3Method vctrs::vec_proxy
vec_proxy.httr2_request <- function(x) {
  x
}

new_aqts_request <- function(x = list(), operation, api) {
  stopifnot(
    is.list(x), #inherits(x, "httr2_request"),
    vapply(x, \(x) inherits(x, "httr2_request"), FUN.VALUE = logical(1)),
    is.character(operation),
    is.character(api)
  )
  operation <- snakecase::to_snake_case(operation)
  operation <- paste0(operation, collapse = "_")
  vctrs::new_list_of(
    x,
    ptype = httr2::request(""),
    operation = operation,
    api = api,
    class = c("aqts_request")
  )
}

validate_aqts_request <- function(x) {
  is_httr2 <- vapply(
    x,
    \(x) inherits(x, "httr2_request"),
    FUN.VALUE = logical(1)
  )
  if (!all(is_httr2)) {
    stop("All elements of x must be `httr2_request` objects")
  }
  x
}

#' @export
format.aqts_request <- function(x, ...) {
  format_one <- function(x) {
    method <- toupper(req_get_method(x))
    cli::format_inline("{.cls {class(x)}} \n {.strong {method}} {x$url} \n")
  }
  vapply(x, format_one, character(1))
}

#' @exportS3Method vctrs::obj_print_data
obj_print_data.aqts_request <- function(x, ...) {
  if (length(x) != 0) {
    n <- pmin(length(x), 5)
    fmt <- format(x[1:n])
    for (i in 1:length(fmt)) {
      cat(paste0("[[", i, "]]\n"))
      print(x[[i]])
      cat("\n")
    }
    if (length(x) > 5) {
      cli::cli_inform("[{.val {n - 5}} more list elements]")
    }
  }
}

# AQTS response objects -----------------------
#' @exportS3Method vctrs::vec_proxy
vec_proxy.httr2_response <- function(x) {
  x
}

new_aqts_response <- function(x, api, operation) {
  vctrs::new_list_of(
    x,
    ptype = httr2::response(url = ""),
    operation = operation,
    api = api,
    class = c("aqts_response")
  )
}

#' @export
format.aqts_response <- function(x) {
  format_one <- function(x) {
    method <- toupper(x$method)
    cli::format_inline("{.cls {class(x)}} \n {.strong {method}} {x$url}\n")
  }
  vapply(x, format_one, character(1))
}

#' @exportS3Method vctrs::obj_print_data
obj_print_data.aqts_response <- function(x, ...) {
  if (length(x) != 0) {
    cat(format(x))
  }
}
# url_last_path <- function(x) {
#   path <- stringr::str_extract(x, "[^/]+/?$")
#   path <- stringr::str_replace(path, "/", "")
#   path
# }

# multi_request <- function(x, req) {
#   foo <- \(x) req |> httr2::req_url_query(!!!x) |> list()
#   x |>
#     dplyr::mutate(row = 1:dplyr::n()) |>
#     dplyr::nest_by(row) |>
#     dplyr::summarize(req = foo(data)) |>
#     dplyr::pull(req)
# }

# new_pdxmaps_request <- function(req) {
#   url <- httr2::req_get_url(req)
#   path <- url_last_path(url)
#   class(req) <- c(paste0("pdx", path, "_request"), class(req))
#   req
# }
#
# new_pdxmaps_response <- function(resp) {
#   path <- httr2::resp_url_path(resp)
#   path <- url_last_path(path)
#   class(resp) <- c(paste0("pdx", path, "_response"), class(resp))
#   resp
# }
