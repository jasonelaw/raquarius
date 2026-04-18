# Aquarius ---------------------------------------------------------------------
# Move code for performing and handling responses out of each function
# Automatically set class of request and response based on "operation"

aq_get_url  <- make_get_env("AQUARIUS_URL")
aq_get_user <- make_get_env("AQUARIUS_USER")
aq_get_pw   <- make_get_env("AQUARIUS_PW")

aquarius <- function(...,
    operation = NULL,
    class = NULL,
    api = c("publish", "acquisition", "provisioning"),
    url = aq_get_url(),
    auth = TRUE,
    .multi = "error"
) {
  verbose <- getOption("raquarius.verbose")
  api    <- match.arg(api)
  path <- switch(api,
    publish      = "publish/v2",
    acquisition  = "acquisition/v2",
    provisioning = "provisioning/v1"
  )

  req <- httr2::request(url) |>
    httr2::req_url_path_append(path, operation) |>
    httr2::req_error(body = error_response) |>
    httr2::req_user_agent("https://github.com/jasonelaw/raquarius") |>
    httr2::req_headers(
      `Accept-Encoding` = "gzip"
    )
  if (auth) {
    req <- req_auth_token(req)
  }
  if (verbose) {
    req <- httr2::req_verbose(req)
  }
  attr(req, "operation") <- operation
  attr(req, "api") <- api



  params <- list2(...)
  vector_args <- any(lengths(params) > 1)
  if (vector_args) {
  req <- multi_request(as_tibble(params), req)
  } else {
  req <- req |> req_url_query(!!!params)
  }
  req <- structure(req, class = c(class, "aqts_request", class(req)))
  return(req)
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

req_auth_token <- function(req, username = aq_get_user(), password = aq_get_pw()) {
  token_exists <- exists_token()
  if (token_exists) {
    token <- get_token()
    is_expired <- token_has_expired(token)
  }
  if (!token_exists || is_expired) {
    token <- PostSession(username, password)
    cache_token(token)
  }
  req <- req |>
    httr2::req_headers_redacted(
      `X-Authentication-Token` = token$token
    )
  req
}


req_perform_aqts <- function(req) {
  if (inherits(req, "aqts_request")) {
    response <- httr2::req_perform(req)
    cls <- setdiff(class(req), c("aqts_request", "httr2_request"))
    ret <- new_aqts_response(response, class = cls)
  }
  # } else if (inherits(req, "list") && length(req) > 1) {
  #   response <- httr2::req_perform_parallel(req, progress = TRUE, max_active = 10)
  #   cls <- map(req, \(x) setdiff(class(x), c("aqts_request", "httr2_request")))
  #   ret <- map2(response, cls, \(x, y) new_aqts_response(x, y))
  # }
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

get_api <- function(url) {
  #url <- httr2::req_get_url(req)
  path <- httr2::url_parse(url)$path
  api_regex <- "[/](publish|provisioning|acquisition)[/]"
  stringr::str_extract(path, api_regex, group = 1)
}

get_operation <- function(url) {
  path <- httr2::url_parse(url)$path
  path <- stringr::str_extract(path, "([^/]+)/?$", group = 1)
  #path <- stringr::str_replace(path, "/", "")
  path
}

new_aqts_request <- function(x, operation, api, class = NULL) {
  stopifnot(inherits(x, "httr2_request"))
  attr(req, "operation") <- operation
  attr(req, "api") <- api
  structure(req, class = c(class, "aqts_request", class(req)))
}

new_aqts_response <- function(x, class = NULL) {
  stopifnot(inherits(x, "httr2_response"))
  new_class <- c(class, "aqts_response", class(x))
  structure(x,
    class = new_class
  )
}

url_last_path <- function(x) {
  path <- stringr::str_extract(x, "[^/]+/?$")
  path <- stringr::str_replace(path, "/", "")
  path
}

handle_request <- function(req, query, .format) {

}

multi_request <- function(x, req) {
  foo <- \(x) req |> httr2::req_url_query(!!!x) |> list()
  x |>
    dplyr::mutate(row = 1:n()) |>
    dplyr::nest_by(row) |>
    dplyr::summarize(req = foo(data)) |>
    dplyr::pull(req)
}

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
