# Web Portal API Wrapper -------------------------------------------------------
wp_get_url <- make_get_env("AQUARIUS_WEBPORTAL_URL")
wp_get_user <- make_get_env("AQUARIUS_WEBPORTAL_USER")
wp_get_pw <- make_get_env("AQUARIUS_WEBPORTAL_PW")


# Web portal paths to implement
# filters,

webportal <- function(...,
                      path = NULL,
                      class = NULL,
                      url = wp_get_url(),
                      username = wp_get_user(),
                      password = wp_get_pw(),
                      .multi = "explode"
) {
  params <- rlang::list2(...)
  verbose <- getOption("raquarius.verbose")
  req <- httr2::request(url) |>
    httr2::req_url_path_append("api", "v1", path) |>
    httr2::req_user_agent("https://github.com/jasonelaw/raquarius") |>
    httr2::req_auth_basic(username, password) |>
    httr2::req_url_query(!!!params, .multi = "explode") |>
    httr2::req_headers(
      `Accept-Encoding` = "gzip"
    )
  if (verbose) {
    req <- httr2::req_verbose(req)
  }
  structure(req, class = c(class, "wp_request", class(req)))
}

resp_body_wp <- function(resp, query = NULL, max_simplify_lvl = 3L) {
  ret <- RcppSimdJson::fparse(
    json = httr2::resp_body_raw(resp),
    query = query,
    max_simplify_lvl = max_simplify_lvl
  )
  ret[c("ResponseStatus")] <- NULL
  ret
}

webportal_perform <- function(req, extract = NULL, as_tibble = TRUE){
  cli::cli_alert_info(c("Retrieving response from {.url {req$url}}"))
  resp <- req |>
    req_perform() |>
    resp_body_string() |>
    jsonlite::fromJSON()
  cli::cli_alert_success("Finished!")
  #cli::cli_alert_info("{resp$Summary}")
  resp$Status <- NULL
  ret <- resp[[extract]]
  if(as_tibble) {
    ret <- tibble::as_tibble(ret)
  }
  ret
}

new_wp_response <- function(x, class) {
  stopifnot(inherits(x, "httr2_response"))
  #path <- httr2::resp_url_path(x)
  new_class <- c(class, "wp_response", class(x))
  structure(x, class = new_class)
}

req_perform_wp <- function(x, max_active = 10, on_error = "stop") {
  is_request <- inherits(x, "httr2_request")
  if (is_request) {
    return(new_wp_response(httr2::req_perform(x), class = class(x)[1]))
  }
  is_list_of_requests <- is.list(x) &&
    all(sapply(x, \(x) inherits(x, "httr2_request")))
  this_class <- unique(sapply(x, \(x) class(x)[1]))
  stopifnot(is_list_of_requests, identical(length(this_class), 1L))

  response <- req_perform_parallel(x, max_active = max_active, on_error = on_error)
  map(response, \(x) new_wp_response(x, class = this_class))
}
