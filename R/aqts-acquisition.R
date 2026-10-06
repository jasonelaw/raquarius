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

#' Append Timeseries
#'
#' These functions append data to timeseries. `aqAppendTimeSeries` can be used
#' to append data to basic timeseries. No existing data will be overwritten.
#' `aqOverwriteAppendTimeSeries` will overwrite existing data
#' @param UniqueId The guid of the timeseries
#' @param Points a data.frame with data to write; must have at least `Time` and `Value` columns; `Type` ("Point" or "Gap"), `GradeCode` (numeric), and `Qualifiers` (character vector) are also allowed
#' @param Start the beginning of the time range to overwrite
#' @param End the end of the time range to overwrite
#' @param open should the `End` argument be considered an open (exclusive) or closed (inclusive) interval
#' @inheritParams perform_and_format
#' @name append
NULL

#' @rdname append
#' @export
aqAppendTimeseries <- function(UniqueId, Points, .perform = TRUE) {
  stopifnot(
    "Points must be a data.frame" = is.data.frame(Points),
    "Points must have columns: Time, Value" = c("Time", "Value") %in% names(Points)
  )
  ret <- aquarius(api = "acquisition") |>
    req_template("/timeseries/{UniqueId}/append") |>
    req_body_json(list(Points = Points))
  if (.perform) {
    ret <- req_perform_aqts(ret)
  }
  ret
}

# "TimeRange":{"Start":"2017-03-01T00:00:00Z","End":"2017-04-01T00:00:00Z"}}

#' @rdname append
#' @export
aqOverwriteAppendTimeseries <- function(UniqueId, Start, End, Points, open = TRUE, .perform = TRUE) {
  stopifnot(
    "Points must be a data.frame" = is.data.frame(Points),
    "Points must have columns: Time, Value" = c("Time", "Value") %in% names(Points),
    "Start and End must be `POSIXct` objects" =
      lubridate::is.POSIXct(Start) & lubridate::is.POSIXct(End),
    "The `open` argument must be a logical of length one" =
      is.logical(open) & identical(length(open), 1L)
  )
  Start <- format_ISO8601(Start, usetz = TRUE)
  End <- format_ISO8601(End, usetz = TRUE)
  if (open) {
    time_range <- list(TimeRange = list(Start = Start, End = End))
  } else {
    time_range <- list(TimeRange = list(Start = Start, InclusiveEnd = End))
  }
  ret <- aquarius(api = "acquisition") |>
    req_template("/timeseries/{UniqueId}/overwriteappend") |>
    req_body_json(list(TimeRange = TimeRange, Points = Points))
  if (.perform) {
    ret <- req_perform_aqts(ret)
  }
  ret
}

#' @rdname append
#' @export
aqAppendReflectedTimeseries <- function(UniqueId, Start, End, Points, open = TRUE, .perform = TRUE) {
  ret <- aqOverwriteAppendTimeseries(
    UniqueId, Start, End, Points, open,
    .perform = FALSE
  ) |>
    req_template("timeseries/{UniqueId}/reflected")
  if (.perform) {
    ret <- req_perform_aqts(ret)
  }
  ret
}
