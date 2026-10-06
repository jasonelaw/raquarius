

#' Get a monitoring location
prGetLocation <- function(unique_id, .perform = TRUE, .format = TRUE){
  ret <- aquarius(api = "provisioning", class = "locations") |>
    req_template("/locations/{unique_id}")
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

# Request factory -------------------------------------------------------------
pr_request_factory <- function(template, formals) {
  formals <- c(formals, rlang::pairlist2(.perform = TRUE))
  f <- function() {
    ret <- aquarius(api = "provisioning") |>
      httr2::req_template(template)
    handle_request(ret, ..., .perform = .perform, .format = TRUE)
  }
  formals(f) <- formals
  f
}

getLocation <- pr_request_factory(
  template = "locations/{LocationUniqueId}",
  formals = alist(LocationUniqueId = )
)

#' Get items nested under locations
#'
#' @param LocationUniqueId the guid for a location
#'
getLocationTimeseries <- pr_request_factory(
  template = "locations/{LocationUniqueId}/timeseries",
  formals = alist(LocationUniqueId = )
)

#' Get items nested under locations
#'
#' @param LocationUniqueId the guid for a location
getLocationDatum <- pr_request_factory(
  template = "locations/{LocationUniqueId}/standardreferencedatums",
  formals = alist(LocationUniqueId = , query = "/Results")
)
