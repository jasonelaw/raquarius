#' Get a monitoring location
prGetLocation <- function(unique_id, .perform = TRUE, .format = TRUE){
  ret <- aquarius(api = "provisioning", class = "locations") |>
    req_template("GET /locations/{unique_id}")
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

#' Delete a location
#'
#' Deletes a monitoring location. Only an empty location can be deleted.
#' @export
prDeleteLocation <- function(unique_id, .perform = TRUE){
  ret <- aquarius(api = "provisioning", class = "locations") |>
    req_template("DELETE /locations/{unique_id}")
  if (.perform) {
    ret <- req_perform_aqts(ret)
  }
  ret
}
