connect_get_url <- make_get_env("AQUARIUS_CONNECT_URL")
connect_get_user <- make_get_env("AQUARIUS_CONNECT_USER")
connect_get_pw <- make_get_env("AQUARIUS_CONNECT_PW")

aqconnect <- function(
    path,
    url = connect_get_url(),
    username = connect_get_user(),
    password = connect_get_pw()
) {
  cookie_file <- fs::path(
    rappdirs::user_data_dir("raquarius"),
    "connect_cookies",
    ext = "txt"
  )
  verbose <- getOption("raquarius.verbose")
  req <- httr2::request(url) |>
    httr2::req_url_path_append("api", path) |>
    httr2::req_user_agent("https://github.com/jasonelaw/raquarius") |>
    httr2::req_cookie_preserve(cookie_file)
  req
}

connectLogin <- function(){
  aqconnect(c("credentials", "login")) |>
    req_body_json(
      data = list(
        username = connect_get_user(),
        password = connect_get_pw()
      )
    ) |>
    req_perform()
}

connectLogout <- function() {
  aqconnect("logout") |>
    req_perform()
}

connectHealth <- function() {
  ret <- aqconnect(c("health")) |>
    req_perform() |>
    resp_body_string()
  cli::cli_inform(c(i = "Aquarius Connect health: {ret}"))
}

connectDbHealth <- function() {
  ret <- aqconnect(c("health", "db")) |>
    req_perform() |>
    resp_body_string()
  cli::cli_inform(c(i = "Aquarius Connect database health: {ret}"))
}

connectSettingsHealth <- function() {
  ret <- aqconnect(c("health", "settings")) |>
    req_perform() |>
    resp_body_string()
  cli::cli_inform(c(i = "Aquarius Connect settings health:", i = "{ret}"))
}

connectToken <- function(){
  response <- aqconnect(c("provisioning", "token")) |>
    req_perform()
  resp_body_json(response)$token
}

provision <- function() {
  aqconnect(c("api", "provisioning")) |>
    req_body_form()
}

# Objects ----------------------------------------------------------------------
Locations <- function(x) {
  cols <- c("Identifier", "Name", "Description", "UtcOffset")
  stopifnot(setequal(names(x), cols))
  n <- nrow(x)
  list(Locations = lapply(1:n, \(i) as.list(x[i,cols])))
}

TriggerOnce <- function(at) {
  ret <- tibble::new_tibble(
    list(
      type = "Once",
      atDateTime = format(at),
      utcOffset = format(at, "%z")
    ),
    class = "Trigger"
  )
  jsonlite::unbox(ret)
}

TriggerDaily <- function(step, from, to, utc_offset) {
  # stopifnot(
  #   hms::is_hms(step),
  #   hms::is_hms(from),
  #   hms::is_hms(to)
  # )
  ret <- list(
    type = "Daily",
    stepTimeOfDay = step,
    fromTimeOfDay = rlang::maybe_missing(from),
    toTimeOfDay   = rlang::maybe_missing(to),
    utcOffset     = rlang::maybe_missing(utc_offset)
  )
  ret <- Filter(Negate(is_missing), ret)
  ret[-1] <- lapply(ret[-1], format)
  ret <- tibble::new_tibble(
    ret,
    class = "Trigger",
    nrow = 1L
  )
  jsonlite::unbox(ret)
}

Schedule <- function(name, ..., description, enabled) {
  triggers = list(...)
  ret <- list(
    name,
    Description = maybe_missing(description),
    Enabled = rlang::maybe_missing(enabled),
    Triggers = list(triggers)
  )
  ret <- Filter(Negate(is_missing), ret)
  ret <- tibble::new_tibble(
    ret,
    class = "Schedule",
    nrow = 1L
  )
  jsonlite::unbox(ret)
}

