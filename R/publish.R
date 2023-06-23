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
GetLocationDescriptionList <- function(...) {
  req <- aq_publish_req("GetLocationDescriptionList", ...)
  req |>
    aquarius_perform("LocationDescriptions")
}

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

#' @export
GetFieldVisitDescriptionList <- function(...) {
  req <- aq_publish_req("GetFieldVisitDescriptionList", ...)
  req |>
    aquarius_perform("FieldVisitDescriptions")
}

#' @export
GetFieldVisitData <- function(...) {
  req <- aq_publish_req("GetFieldVisitData", ...)
  req |>
    aquarius_perform("FieldVisitData")
}

#' @export
GetFieldVisitDataByLocation <- function(...) {
  req <- aq_publish_req("GetFieldVisitDataByLocation", ...)
  req |>
    aquarius_perform("FieldVisitData")
}

# Time Series Requests ---------------------------------------------------------

#' @export
GetTimeSeriesUniqueIdList <- function(...) {
  req <- aq_publish_req("GetTimeSeriesUniqueIdList", ...)
  req |>
    aquarius_perform("TimeSeriesUniqueIds")
}

#' @export
GetTimeSeriesDescriptionListByUniqueId <- function(req, TimeSeriesUniqueIds) {
  ids <- paste0("TimeSeriesUniqueIds=",TimeSeriesUniqueIds, collapse = "&")
  req <- aq_publish_req("GetTimeSeriesDescriptionListByUniqueId") |>
    req_method("POST") |>
    req_body_raw(ids, type = "application/x-www-form-urlencoded")
  req |>
    aquarius_perform("TimeSeriesDescriptions")
}

#' @export
GetTimeSeriesData <- function(...) {
  req <- aq_publish_req("GetTimeSeriesData", ...)
  resp <- aquarius_perform(req, as_tibble = FALSE)
  points <- pivot_longer(
    data = resp$Points,
    cols = -Timestamp,
    names_to = c(".value", "ts"),
    names_pattern = "([a-zA-Z]+)([0-9]{1,2})"
  ) |>
    group_by(ts) |>
    nest()
  tibble(resp$TimeSeries, Points = points$data)
}

#' @export
GetApprovalsTransactionList <- function(...){
  req <- aq_publish_req("GetApprovalsTransactionList", ...)
  req |>
    aquarius_perform("ApprovalsTransactions")
}
