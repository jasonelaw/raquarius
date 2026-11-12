#' Shared parameter definitions
#'
#' @param ... additional arguments passed to the endpoint - please see the AQTS API documentation for available arguments
#' @param .perform  if TRUE, perform the query and return the response
#' @param .format if TRUE and `.perform = TRUE`, format the response, usually as a `data.frame`
#' @name perform_and_format
NULL

# Authentication ---------------------------------------------------------------
GetSessionPublickey <- function() {
  resp <- aqts("publish", c("session", "publickey")) |>
    req_perform()
  as.pubkey(resp)
}

PostSession <- function(
  pubkey,
  username = aq_get_user(),
  password = aq_get_pw()
) {
  if (!is.raw(password)) {
    password <- openssl::rsa_encrypt(charToRaw(password), pubkey, TRUE)
  }
  token <- aqts("publish", "session") |>
    req_body_json(
      data = list(Username = username, EncryptedPassword = password)
    ) |>
    req_perform() |>
    resp_body_string()
  token <- new_aqtoken(token)
  token
}

DeleteSession <- function() {
  req <- aqts("publish", "session") |>
    req_method("DELETE")
  resp <- req_perform(req)
  if (resp_status(resp) < 300) {
    the$reset()
  }
  resp
}

GetSessionKeepalive <- function() {
  req <- aqts("publish", c("session", "keepalive")) |>
    req_auth_token()
  resp <- req_perform(req)
  if (resp_status(resp) < 300) {
    key <- hash(aq_get_url())
    token <- the$get(key)
    the$set(key, new_aqtoken(token$token))
  }
  resp
}
# System wide configuration item retrieval -------------------------------------

#' Functions for retrieving configuration items
#'
#' These functions retrieve "system-wide configuration items". These are look up
#' lists that are used as allowed values throughout Aquarius.
#' @name publish-config
#' @param ... pass query arguments to route. Please see Publish API documentation
#' for available arguments.
NULL

#' @describeIn publish-config Retrieve parameter list.
#' @export
GetParameterList <- function(.format = TRUE, .perform = TRUE) {
  ret <- aquarius(operation = "GetParameterList")
  handle_request(ret, .perform, .format)
}

#' @describeIn publish-config Retrieve monitoring methods.
#' @export
GetMonitoringMethodList <- function(.format = TRUE, .perform = TRUE) {
  ret <- aquarius(operation = "GetMonitoringMethodList")
  handle_request(ret, .perform, .format)
}

#' @describeIn publish-config Retrieve units.
#' @param GroupIdentifier Filter to a specific Unit Group.
#' @export
GetUnitList <- function(
  GroupIdentifier = NULL,
  .format = TRUE,
  .perform = TRUE
) {
  ret <- aquarius(GroupIdentifier = GroupIdentifier, operation = "GetUnitList")
  handle_request(ret, .perform, .format)
}

#' @describeIn publish-config Retrieve approvals.
#' @export
GetApprovalList <- function(.format = TRUE, .perform = TRUE) {
  ret <- aquarius(operation = "GetApprovalList")
  handle_request(ret, .perform, .format)
}

#' @describeIn publish-config Retrieve grades.
#' @export
GetGradeList <- function(.format = TRUE, .perform = TRUE) {
  ret <- aquarius(operation = "GetGradeList")
  handle_request(ret, .perform, .format)
}

#' @describeIn publish-config Retrieve qualifiers list.
#' @export
GetQualifierList <- function(.format = TRUE, .perform = TRUE) {
  ret <- aquarius(operation = "GetQualifierList")
  handle_request(ret, .perform, .format)
}
#' @export
#' @describeIn publish-config tag applicability
TagApplicability <- rlang::set_names(c(
  "AppliesToLocations",
  "AppliesToLocationNotes",
  "AppliesToSensorsGauges",
  "AppliesToAttachments",
  "AppliesToReports"
))

#' @describeIn publish-config Retrieve tags
#' @param Applicability tag applicability, one of  "AppliesToLocations",
#' "AppliesToLocationNotes", "AppliesToSensorsGauges","AppliesToAttachements",
#' "AppliesToReports"
#' @export
GetTagList <- function(Applicability = NULL, .format = TRUE, .perform = TRUE) {
  if (!is.null(Applicability)) {
    Applicability <- rlang::arg_match(Applicability, TagApplicability)
  }
  ret <- aquarius(Applicability = Applicability, operation = "GetTagList")
  handle_request(ret, .perform, .format)
}

#' @rdname publish-config
#' @export
GetActiveMetersAndCalibrations <- function(.format = TRUE, .perform = TRUE) {
  ret <- aquarius(operation = "GetActiveMetersAndCalibrations")
  handle_request(ret, .perform, .format)
}

# Report List ------------------------------------------------------------------
#' Query Reports
#'
#' Queries a
#'
#' @export
GetReportList <- function(
  LocationUniqueId = NULL,
  TimeSeriesUniqueIds = NULL,
  ReportTitle = NULL,
  ...,
  .format = TRUE,
  .perform = TRUE
) {
  ret <- aquarius(
    LocationUniqueId = LocationUniqueId,
    TimeSeriesUniqueIds = TimeSeriesUniqueIds,
    ReportTitle = ReportTitle,
    operation = "GetReportList"
  )
  handle_request(ret, .perform, .format)
}

# Location Requests ------------------------------------------------------------
#' Functions for Locations
#'
#' These functions retrieve location information.
#' @name location-requests
#' @param ... pass query arguments to route. Please see Publish API documentation
#' for available arguments.
NULL

#' @rdname location-requests
#' @export
GetLocationDescriptionList <- function(
  LocationIdentifier = NULL,
  ...,
  .format = TRUE,
  .perform = TRUE
) {
  ret <- aquarius(
    ...,
    LocationIdentifier = LocationIdentifier,
    operation = "GetLocationDescriptionList"
  )
  handle_request(ret, .perform, .format)
}
#' @rdname location-requests
#' @export
GetLocationData <- function(
  LocationIdentifier,
  ...,
  .format = TRUE,
  .perform = TRUE,
  .parallel = FALSE
) {
  ret <- aquarius(
    ...,
    LocationIdentifier = LocationIdentifier,
    operation = "GetLocationData"
  )
  handle_request(ret, .perform, .format, .parallel)
}

# Field Visit Requests ---------------------------------------------------------
#' Publish API: Functions for Field Visits
#'
#' These functions retrieve field visit information via the AQTS Publish API
#' @name fieldvisit-requests
#' @param ... pass query arguments to route. Please see Publish API documentation
#' for available arguments.
NULL

#' @rdname fieldvisit-requests
#' @export
GetFieldVisitDescriptionList <- function(
  ...,
  LocationIdentifier = NULL,
  QueryFrom = NULL,
  QueryTo = NULL,
  .format = TRUE,
  .perform = TRUE
) {
  ret <- aquarius(
    ...,
    LocationIdentifier = LocationIdentifier,
    QueryFrom = QueryFrom,
    QueryTo = QueryTo,
    operation = "GetFieldVisitDescriptionList"
  )
  handle_request(ret, .perform, .format)
}

#' @rdname fieldvisit-requests
#' @export
GetFieldVisitData <- function(
  FieldVisitIdentifier,
  ...,
  .format = TRUE,
  .perform = TRUE
) {
  ret <- aquarius(
    ...,
    FieldVisitIdentifier = FieldVisitIdentifier,
    operation = "GetFieldVisitData",
    class = "fvdata"
  )
  handle_request(ret, .perform, .format)
}

#' @rdname fieldvisit-requests
#' @export
GetFieldVisitDataByLocation <- function(
  LocationIdentifier,
  Parameters = NULL,
  Activities = NULL,
  ...,
  .format = TRUE,
  .perform = TRUE
) {
  # if (!is.null(Activities)) {
  #   Activities <- rlang::arg_match(
  #     Activities,
  #     c("Reading", "Inspection", "CalibrationCheck")
  #   )
  # }
  ret <- aquarius(
    ...,
    LocationIdentifier = LocationIdentifier,
    Parameters = Parameters,
    Activities = Activities,
    operation = "GetFieldVisitDataByLocation"
  )
  handle_request(ret, .perform, .format)
}

#' @rdname fieldvisit-requests
#' @export
GetFieldVisitReadingsByLocation <- function(
  LocationIdentifier = NULL,
  LocationUniqueId = NULL,
  Parameters = NULL,
  ...,
  .format = TRUE,
  .perform = TRUE
) {
  stopifnot(
    `One of LocationIdentifier or LocationUniqueId must be specified` = xor(
      is.null(LocationIdentifier),
      is.null(LocationUniqueId)
    )
  )

  ret <- aquarius(
    ...,
    LocationIdentifier = LocationIdentifier,
    LocationUniqueId = LocationUniqueId,
    Parameters = Parameters,
    operation = "GetFieldVisitReadingsByLocation"
  )
  handle_request(ret, .perform, .format)
}

# Time Series Requests ---------------------------------------------------------
#' Retrieve Time Series Information From Publish API
#'
#' These functions retrieve time series information.
#' @param TimeSeriesUniqueId a unique id
#' @param ... pass query arguments to route. Please see Publish API documentation
#' for available arguments.
#' @name ts-requests
NULL

#' @rdname ts-requests
#' @export
GetTimeSeriesUniqueIdList <- function(
  LocationIdentifier = NULL,
  Parameter = NULL,
  ...,
  .format = TRUE,
  .perform = TRUE
) {
  ret <- aquarius(
    ...,
    LocationIdentifier = LocationIdentifier,
    Parameter = Parameter,
    operation = "GetTimeSeriesUniqueIdList"
  )
  handle_request(ret, .perform, .format)
}

#' @rdname ts-requests
#' @export
GetTimeSeriesDescriptionList <- function(
  LocationIdentifier = NULL,
  Parameter = NULL,
  ...,
  .format = TRUE,
  .perform = TRUE
) {
  ret <- aquarius(
    ...,
    LocationIdentifier = LocationIdentifier,
    Parameter = Parameter,
    operation = "GetTimeSeriesDescriptionList"
  )
  handle_request(ret, .perform, .format)
}

#' @rdname ts-requests
#' @export
GetTimeSeriesDescriptionListByUniqueId <- function(
  TimeSeriesUniqueIds,
  .format = TRUE,
  .perform = TRUE
) {
  ret <- aquarius(
    operation = "GetTimeSeriesDescriptionListByUniqueId"
  )
  ret <- map(
    ret,
    \(req) {
      req |>
        req_body_json(
          data = list(TimeSeriesUniqueIds = TimeSeriesUniqueIds)
        )
    }
  ) |>
    vctrs::vec_restore(ret)
  handle_request(ret, .perform, .format)
}

#' Get Time Series Data
#'
#' Gets time series data that is time aligned to the first time series in the
#' request.
#'
#' This is one endpoint where wrapping vs not wrapping the TimeSeriesUniqueIds
#' argument in a list() makes a very large difference. Passing
#' TimeSeriesUniqueIds = list(1:5) vs TimeSeriesUniqueIds = 1:5 results in 1
#' versus 5 requests. In the former, each of the ts ids 1:5 will be time aligned
#' to ts=1 (the timestamps returned will only occur when ts=1 has values)
#' whereas in the latter, each ts will be time aligned to itself (the timestamps
#' will be returned whenever any of the timeseries have values).
#' @rdname ts-requests
#' @export
GetTimeSeriesData <- function(
  TimeSeriesUniqueIds,
  QueryFrom = NULL,
  QueryTo = NULL,
  ...,
  .format = TRUE,
  .perform = TRUE,
  .parallel = FALSE
) {
  ret <- aquarius(
    ...,
    TimeSeriesUniqueIds = TimeSeriesUniqueIds,
    QueryFrom = QueryFrom,
    QueryTo = QueryTo,
    operation = "GetTimeSeriesData"
  )
  handle_request(ret, .perform, .format)
}

#' @rdname ts-requests
#' @export
GetApprovalsTransactionList <- function(
  TimeSeriesUniqueId = NULL,
  QueryFrom = NULL,
  QueryTo = NULL,
  ...,
  .format = TRUE,
  .perform = TRUE
) {
  ret <- aquarius(
    ...,
    TimeSeriesUniqueId = TimeSeriesUniqueId,
    QueryFrom = QueryFrom,
    QueryTo = QueryTo,
    operation = "GetApprovalsTransactionList",
    .names = "TimeSeriesUniqueId"
  )

  handle_request(ret, .perform, .format)
}

#' @rdname ts-requests
#' @export
GetTimeSeriesCorrectedData <- function(
  TimeSeriesUniqueId,
  QueryFrom = NULL,
  QueryTo = NULL,
  Unit = NULL,
  ...,
  .format = TRUE,
  .perform = TRUE,
  .parallel = FALSE
) {
  ret <- aquarius(
    ...,
    TimeSeriesUniqueId = TimeSeriesUniqueId,
    QueryFrom = QueryFrom,
    QueryTo = QueryTo,
    Unit = Unit,
    operation = "GetTimeSeriesCorrectedData"
  )

  handle_request(ret, .perform, .format)
}

#' @rdname ts-requests
#' @export
GetMetadataChangeTransactionList <- function(
  TimeSeriesUniqueId,
  ...,
  .perform = TRUE,
  .format = TRUE
) {
  ret <- aquarius(
    ...,
    TimeSeriesUniqueId = TimeSeriesUniqueId,
    operation = "GetMetadataChangeTransactionList"
  )
  handle_request(ret, .perform, .format)
}
