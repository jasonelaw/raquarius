#' Create an Aquarius web service token
#'
#' Creates a S3 object of class `<aq_token>` representing a token
#' returned from POST /publish/v2/session.
#'
#' @param access_token The access token used to authenticate request
#' @param expires_in Number of seconds until token expires.
#' @param .date Date the request was made; used to convert the relative
#'   `expires_in` to an absolute `expires_at`.
#' @return An X-Authentication-Token for Aquarius Time Series: an S3 list with class `aq_token`.
#' @export
#' @examples
#' new_aqtoken("abcdef")
#' new_aqtoken("abcdef", expires_in = 3600)
new_aqtoken <- function(access_token, expires_in = 3600, .date = Sys.time()) {

  check_string(access_token)
  check_number_whole(expires_in, allow_null = FALSE)

  if (!is.null(expires_in)) {
    # Store as unix time to avoid worrying about type coercions in cache
    expires_at <- as.numeric(.date) + expires_in
  } else {
    expires_at <- NULL
  }

  structure(
    compact(list2(
      token = access_token,
      expires_at = expires_at
    )),
    class = "aqtoken"
  )
}

#' @export
print.aqtoken <- function(x, ...) {
  oldx <- x
  x$expires_at <- format(as.POSIXct(x$expires_at))
  cli::cli_text(cli::style_bold("<", paste(class(x), collapse = "/"), ">"))
  cli::cli_dl(x)
  invisible(oldx)
}

token_has_expired <- function(token, delay = 5) {
  if (is.null(token$expires_at)) {
    FALSE
  } else {
    (unix_time() + delay) > token$expires_at
  }
}

unix_time <- function() {
  as.integer(Sys.time())
}

# token cache, `the`, is initialized in raquarius-package.R from cachem::cache_mem()
cache_token <- function(token) {
  the$set(hash(aq_get_url()), token)
}

get_token <- function() {
  key <- hash(aq_get_url())
  stopifnot(the$exists(key))
  the$get(key)
}

exists_token <- function() {
  key <- hash(aq_get_url())
  the$exists(key)
}
