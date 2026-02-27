#'Returns an object suitable for the Datasets argument of the bulk export endpoints
#'@export
Datasets <- function(Identifier, Calculation = NULL, Unit = NULL){
  if (!is.null(Calculation)){
    choices <- c("Instantaneous", "Maximum", "Minimum", "Aggregate",
      "Integrate", "Differentiate")
    stopifnot(Calculation %in% choices)
  }
  tibble(Identifier, Calculation, Unit)
}

make_get_env <- function(keyname, signal_error = TRUE) {
  function() {
    key <- Sys.getenv(keyname, unset = NA)
    if (is.na(key) && signal_error) {
      msg <- glue::glue("{ keyname } key not found, please set { keyname } env var using Sys.setenv or .Rprofile")
      stop(msg)
    }
    key
  }
}

process_pars <- function(...) {
  pars <- list2(...)
  pars <- Filter(\(x) !is.null(x), pars)
  pars <- pars_flatten(pars)
  pars
}

pars_flatten <- function(x){
  lx <- sapply(x, length)
  i  <- lx > 1
  nms <- names(x)
  x[i] <- lapply(x[i], as.list)
  setNames(
    object = c(x, recursive = TRUE),
    nm     = rep(nms, lx)
  )
}

parse_xml_pubkey <- function(xml) {
  xml_key <- xml2::read_xml(xml)
  xml_key <- xml2::as_list(xml_key, encoding = "UTF-8")
  create_pubkey(
    exponent = xml_key$RSAKeyValue$Exponent[[1]],
    modulus  = xml_key$RSAKeyValue$Modulus[[1]]
  )
}

# arguments are base64 encoded
create_pubkey <- function(exponent, modulus){
  modulus  <- openssl::base64_decode(modulus)
  exponent <- openssl::base64_decode(exponent)
  modulus  <- openssl::bignum(modulus)
  exponent <- openssl::bignum(exponent)
  pubkey <- openssl:::rsa_pubkey_build(exponent, modulus)
  structure(pubkey, class = c("pubkey", "rsa"))
}

keepalive_token <- function(){
  keepalive <- function(){
    cli::cli_alert_info("Keeping session alive")
    request(aq_get_url()) |>
      req_url_path_append("publish", "v2", "session", "keepalive") |>
      req_headers(
        `Accept-Encoding`        = "gzip",
        `X-Authentication-Token` = Sys.getenv("AQUARIUS_AUTH_TOKEN")
      ) |>
      req_perform()
  }
  next.keepalive <- later::next_op_secs()
  if(identical(next.keepalive, Inf)){
    later::later(keepalive, 60*30)
  }
}

check_host_reachable <- function(url) {
  url <- httr2::url_parse(url)
  dns <- curl::nslookup(url$hostname, error = FALSE)
  if(is.null(dns)) {
    cli::cli_abort(
      message = "Cannot reach host {url$hostname}. Check your internet or VPN connection if attempting to reach a private network.",
      call = caller_env(n = 2)
    )
  }
}

parse_timestamp <- function(x) {
  allna <- all(is.na(x))
  if(allna){
    x <- as.character(x)
  }
  ret <- lubridate::fast_strptime(x, "%Y-%m-%dT%H:%M:%OS%z", lt = FALSE)
  lubridate::with_tz(ret, tzone = Sys.timezone())
}

.setRAquariusOptions <- function(pkgname) {
  options(
    "raquarius.verbose" = FALSE
  )
}
