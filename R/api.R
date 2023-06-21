

# Aquarius ---------------------------------------------------------------------
aquarius <- function(url, username, password) {
  url  <- if (missing(url))      Sys.getenv("AQUARIUS_URL")
  user <- if (missing(username)) Sys.getenv("AQUARIUS_USER")
  pw   <- if (missing(password)) Sys.getenv("AQUARIUS_PW")
  request(url) |>
    req_error(is_error = \(x) resp_status(x) >= 400, body = \(x) resp_body_json(x)$ResponseStatus$Message) |>
    req_user_agent("https://github.com/jasonelaw/raquarius") |>
    req_auth_basic(user, pw)
}

aq_publish_req <- function(path, ...) {
  pars <- process_pars(...)
  aquarius() |>
    req_url_path_append("publish", "v2", path) |>
    req_url_query(!!!pars)
}

aquarius_perform <- function(req, extract = NULL, as_tibble = TRUE){
  resp <- req |>
    req_perform() |>
    resp_body_string() |>
    jsonlite::fromJSON()
  resp$Summary <- NULL
  resp$ResponseTime <- NULL
  resp$ResponseVersion <- NULL
  if(as_tibble){
    resp <- as_tibble(if(is.null(extract)) resp else resp[[extract]])
  }
  resp
}

# Web Portal -------------------------------------------------------------------
webportal <- function(url, username, password) {
  url  <- if (missing(url))      Sys.getenv("AQUARIUS_WEBPORTAL_URL")
  user <- if (missing(username)) Sys.getenv("AQUARIUS_WEBPORTAL_USER")
  pw   <- if (missing(password)) Sys.getenv("AQUARIUS_WEBPORTAL_PW")
  request(url) |>
    req_user_agent("https://github.com/jasonelaw/raquarius") |>
    # req_error(
    #   is_error = \(x) resp_status(x) >= 400#,
    #   #body     = \(x) resp_body_html(x)
    # ) |>
    req_auth_basic(user, pw)
}

aq_webportal_req <- function(path, ...) {
  pars <- process_pars(...)
  webportal() |>
    req_url_path_append("api", "v1", path) |>
    req_url_query(!!!pars)
}
