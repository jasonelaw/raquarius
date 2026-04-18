# Acquisition API ------------------------------------------------------------

#' Post Attachements
#'
#' These functions post files as attachments.
#' @name post-attachments
#' @param ... pass query arguments to route. Please see Acquisition API documentation
#' for available arguments.
NULL

#' @rdname post-attachments
#' @export
aqPostReportAttachment <- function(File, LocationUniqueId, Title, type, ..., .perform = TRUE) {
  ret <- aquarius(api = "acquisition") |>
    req_template("/locations/{LocationUniqueId}/attachments/reports") |>
    req_body_multipart(
      File = curl::form_file(path = File, type = type),
      Title = Title,
      ...
    )
  if (.perform) {
    ret <- req_perform_aqts(ret)
  }
  ret
}

#' @rdname post-attachments
#' @export
aqDeleteReportAttachment <- function(ReportUniqueId, .perform = TRUE) {
  ret <- aquarius(api = "acquisition") |>
    req_template("/attachments/reports/{ReportUniqueId}") |>
    req_method("DELETE")
  if (.perform) {
    ret <- req_perform(ret)
  }
  ret
}

#' @rdname post-attachments
#' @export
aqPostLocationAttachment <- function(File, LocationUniqueId, ..., .perform = TRUE) {
  ret <- aquarius(api = "acquisition") |>
    req_template("/locations/{LocationUniqueId}/attachments") |>
    req_body_multipart(
      File = curl::form_file(path = File, type = mime::guest_type(File)),
      Title = Title,
      ...
    )
  if (.perform) {
    ret <- req_perform_aqts(ret)
  }
  ret
}

#' @export
aqAppendTimeseries <- function(UniqueId, Points, .perform = TRUE) {
  stopifnot(
    "Points must be a data.frame" = is.data.frame(Points),
    "Points must have columns: Time, Value" = c("Time", "Value") %in% names(Points)
  )
  ret <- aquarius(api = "acquisition") |>
    req_template("/timeseries/{UniqueId}/append") |>
    req_body_json(list(Points = Points)))
  if (.perform) {
    ret <- req_perform_aqts(ret)
  }
  ret
}

aqOverwriteAppendTimeseries <- function() {

}

aqAppendReflectedTimeseries <- function() {

}
