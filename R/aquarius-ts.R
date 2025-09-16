# Aquarius ---------------------------------------------------------------------
aq_get_url <- make_get_env("AQUARIUS_URL")
aq_get_user <- make_get_env("AQUARIUS_USER")
aq_get_pw <- make_get_env("AQUARIUS_PW")



aquarius <- function(...,
    operation = NULL,
    class = NULL,
    api = c("publish", "acquisition", "provisioning"),
    url = aq_get_url(),
    #method = c("GET", "POST", "DELETE", "PUT"),
    auth = TRUE,
    .multi = "error"
) {
  verbose <- getOption("raquarius.verbose")
  api    <- match.arg(api)
  #method <- match.arg(method)
  path <- switch(api,
    publish      = "publish/v2",
    acquisition  = "acquisition/v2",
    provisioning = "provisioning/v1"
  )

  params <- list2(...)
  req <- httr2::request(url) |>
    httr2::req_url_path_append(path, operation) |>
    httr2::req_error(body = error_response) |>
    httr2::req_user_agent("https://github.com/jasonelaw/raquarius") |>
    httr2::req_headers(
      `Accept-Encoding` = "gzip"
    ) |>
    req_url_query(!!!params)
  if (auth) {
    req <- req_auth_token(req)
  }
  if (verbose) {
    req <- httr2::req_verbose(req)
  }
  attr(req, "operation") <- operation
  attr(req, "api") <- api
  structure(req, class = c(class, "aqts_request", class(req)))
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
    req_headers(
      `X-Authentication-Token` = token$token,
      .redact = "X-Authentication-Token"
    )
  req
}

req_perform_aqts <- function(req) {
  response <- httr2::req_perform(req)
  class <- rev(class(req))[3]
  class <- if (is.na(class)) NULL else class
  new_aqts_response(response, class = class)
}

resp_body_aqts <- function(resp, query = NULL, max_simplify_lvl = 0L) {
  ret <- RcppSimdJson::fparse(
    json = resp_body_raw(resp),
    query = query,
    max_simplify_lvl = max_simplify_lvl
  )
  ret[c("Summary", "ResponseTime", "ResponseVersion")] <- NULL
  as_tibble(ret)
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
