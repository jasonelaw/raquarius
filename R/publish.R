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
GetParameterList <- function() {
  req <- aq_publish_req("GetParameterList")
  req |>
    aquarius_perform("Parameters")
}

#' @describeIn publish-config Retrieve monitoring methods.
#' @export
GetMonitoringMethodList <- function() {
  req <- aq_publish_req("GetMonitoringMethodList")
  req |>
    aquarius_perform("MonitoringMethods")
}

#' @describeIn publish-config Retrieve units.
#' @export
GetUnitList <- function(...){
  req <- aq_publish_req("GetUnitList", ...)
  req |>
    aquarius_perform("Units")
}

#' @describeIn publish-config Retrieve approvals.
#' @export
GetApprovalList <- function(){
  req <- aq_publish_req("GetApprovalList")
  req |>
    aquarius_perform("Approvals")
}

#' @describeIn publish-config Retrieve grades.
#' @export
GetGradeList <- function(){
  req <- aq_publish_req("GetGradeList")
  req |>
    aquarius_perform("Grades")
}

#' @describeIn publish-config Retrieve qualifiers list.
#' @export
GetQualifierList <- function(){
  req <- aq_publish_req("GetQualifierList")
  req |>
    aquarius_perform("Qualifiers")
}

#' @describeIn publish-config Retrieve tags.
#' @export
GetTagList <- function(...){
  req <- aq_publish_req("GetTagList", ...)
  req |>
    aquarius_perform("Tags")
}

#' @rdname publish-config
#' @export
GetActiveMetersAndCalibrations <- function(){
  req <- aq_publish_req("GetActiveMetersAndCalibrations")
  req |>
    aquarius_perform("ActiveMeterDetails")
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
GetLocationDescriptionList <- function(...) {
  req <- aq_publish_req("GetLocationDescriptionList", ...)
  req |>
    aquarius_perform("LocationDescriptions")
}

#' @rdname location-requests
#' @export
GetLocationData <- function(...) {
  req <- aq_publish_req("GetLocationData", ...)
  resp <- req |>
    aquarius_perform(as_tibble = FALSE)
  resp <- map(
    .x = Filter(\(x) length(x)>0, resp),
    .f = \(x) if(length(x) > 1) list(x) else x
  )
  as_tibble_row(resp)
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
GetFieldVisitDescriptionList <- function(...) {
  req <- aq_publish_req("GetFieldVisitDescriptionList", ...)
  req |>
    aquarius_perform("FieldVisitDescriptions")
}

#' @rdname fieldvisit-requests
#' @export
GetFieldVisitData <- function(...) {
  req <- aq_publish_req("GetFieldVisitData", ...)
  ret <- req |>
    aquarius_perform(as_tibble = FALSE)
  df <- tibble_row(!!!map_if(ret, is.list, \(x) list(x))) |>
    mutate(
      ApprovalLevel = Approval[[1]]$ApprovalLevel,
      ApprovalLevelDescription = Approval[[1]]$LevelDescription
    ) |>
    select(-Approval)
}

#' @rdname fieldvisit-requests
#' @export
GetFieldVisitDataByLocation <- function(...) {
  req <- aq_publish_req("GetFieldVisitDataByLocation", ...)
  req |>
    aquarius_perform("FieldVisitData")
}

#' @rdname fieldvisit-requests
#' @export
GetFieldVisitReadingsByLocation <- function(...) {
  req <- aq_publish_req("GetFieldVisitReadingsByLocation", ...)
  req |>
    aquarius_perform("FieldVisitReadings")
}

# Time Series Requests ---------------------------------------------------------
#' Retrieve Locations From Publish API
#'
#' These functions retrieve location information.
#' @param ... pass query arguments to route. Please see Publish API documentation
#' for available arguments.
#' @name ts-requests
NULL

#' @rdname ts-requests
#' @export
GetTimeSeriesUniqueIdList <- function(...) {
  req <- aq_publish_req("GetTimeSeriesUniqueIdList", ...)
  req |>
    aquarius_perform("TimeSeriesUniqueIds")
}

#' @rdname ts-requests
#' @export
GetTimeSeriesDescriptionListByUniqueId <- function(req, TimeSeriesUniqueIds) {
  ids <- paste0("TimeSeriesUniqueIds=",TimeSeriesUniqueIds, collapse = "&")
  req <- aq_publish_req("GetTimeSeriesDescriptionListByUniqueId") |>
    req_method("POST") |>
    req_body_raw(ids, type = "application/x-www-form-urlencoded")
  req |>
    aquarius_perform("TimeSeriesDescriptions")
}

#' @rdname ts-requests
#' @export
GetTimeSeriesData <- function(...) {
  req <- aq_publish_req("GetTimeSeriesData", ...)
  resp <- aquarius_perform(req, as_tibble = FALSE)
  if(identical(resp$POints, list())) {
    points <- tibble()
  } else {
    points <- pivot_longer(
      data = resp$Points,
      cols = -Timestamp,
      names_to = c(".value", "ts"),
      names_pattern = "([a-zA-Z]+)([0-9]{1,2})"
    ) |>
      group_by(ts) |>
      nest()
  }
  tibble(resp$TimeSeries, Points = points$data)
}

#' @rdname ts-requests
#' @export
GetApprovalsTransactionList <- function(...){
  req <- aq_publish_req("GetApprovalsTransactionList", ...)
  req |>
    aquarius_perform("ApprovalsTransactions")
}

#' @rdname ts-requests
#' @export
GetTimeSeriesCorrectedData <- function(...){
  req <- aq_publish_req("GetTimeSeriesCorrectedData", ...)
  req |>
    aquarius_perform(as_tibble = FALSE)
}
