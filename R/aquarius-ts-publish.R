# Authentication ---------------------------------------------------------------
GetSessionPublickey <- function(){
  resp <- aquarius(operation = c("session", "publickey"), api = "publish", auth = FALSE) |>
    req_perform() |>
    resp_body_json()
  parse_xml_pubkey(resp[["Xml"]])
}

PostSession <- function(username, password = askpass::askpass()) {
  pubkey <- GetSessionPublickey()
  if(!is.raw(password)) {
    password <- openssl::rsa_encrypt(charToRaw(password), pubkey, TRUE)
  }
  token <- aquarius(operation = "session", api = "publish", auth = FALSE) |>
    req_body_json(
      data = list(Username = username, EncryptedPassword = password)
    ) |>
    req_perform() |>
    resp_body_string()
  token <- new_aqtoken(token)
  token
}

DeleteSession <- function() {
  req <- aquarius(operation = "session", api = "publish") |>
    req_method("DELETE")
  resp <- req_perform(req)
  if(resp_status(resp) < 300){
    the$reset()
  }
  resp
}

GetSessionKeepalive <- function() {
  req <- aquarius(operation = c("session", "keepalive"), api = "publish")
  resp <- req_perform(req)
  if(resp_status(resp) < 300){
    key <- hash(aq_get_url())
    token <- the$get(key)
    the$set(key, aq_token(token$token))
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
 GetParameterList <- function(.format = TRUE, .perform = TRUE){
   ret <- aquarius(operation = "GetParameterList", class = "parameter")
   if (.perform) {
     ret <- req_perform_aqts(ret)
     if (.format){
       ret <- format_response(ret)
     }
   }
   ret
 }

#' @describeIn publish-config Retrieve monitoring methods.
#' @export
GetMonitoringMethodList <- function(.format = TRUE, .perform = TRUE){
   ret <- aquarius(operation = "GetMonitoringMethodList")
   if (.perform) {
     ret <- req_perform_aqts(ret)
     if (.format){
       ret <- format_response(ret, query = "/MonitoringMethods")
     }
   }
   ret
 }

#' @describeIn publish-config Retrieve units.
#' @param GroupIdentifier Filter to a specific Unit Group.
#' @export
GetUnitList <- function(GroupIdentifier = NULL, .format = TRUE, .perform = TRUE){
  ret <- aquarius(GroupIdentifier = GroupIdentifier, operation = "GetUnitList")
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret, query = "/Units")
    }
  }
  ret
}

#' @describeIn publish-config Retrieve approvals.
#' @export
GetApprovalList <- function(.format = TRUE, .perform = TRUE){
  ret <- aquarius(operation = "GetApprovalList")
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret, query = "/Approvals")
    }
  }
  ret
}

#' @describeIn publish-config Retrieve grades.
#' @export
GetGradeList <- function(.format = TRUE, .perform = TRUE){
  ret <- aquarius(operation = "GetGradeList")
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret, query = "/Grades")
    }
  }
  ret
}

#' @describeIn publish-config Retrieve qualifiers list.
#' @export
GetQualifierList <- function(.format = TRUE, .perform = TRUE){
  path <- "GetQualifierList"
  json_key <-
  ret <- aquarius(operation = "GetQualifierList")
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret, query = "/Qualifiers")
    }
  }
  ret
}
#' @export
#' @describeIn publish-config tag applicability
TagApplicability <- rlang::set_names(c(
  "AppliesToLocations", "AppliesToLocationNotes", "AppliesToSensorsGauges",
  "AppliesToAttachments", "AppliesToReports"
))

#' @describeIn publish-config Retrieve tags
#' @param Applicability tag applicability, one of  "AppliesToLocations",
#' "AppliesToLocationNotes", "AppliesToSensorsGauges","AppliesToAttachements",
#' "AppliesToReports"
#' @export
GetTagList <- function(Applicability = NULL, .format = TRUE, .perform = TRUE){
  if(!is.null(Applicability)) {
    Applicability <- rlang::arg_match(Applicability, TagApplicability)
  }
  ret <- aquarius(Applicability = Applicability, operation = "GetTagList")
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret, "/Tags")
    }
  }
  ret
}

#' @rdname publish-config
#' @export
GetActiveMetersAndCalibrations <- function(.format = TRUE, .perform = TRUE){
  ret <- aquarius(operation = "GetActiveMetersAndCalibrations")
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret, query = "/ActiveMeterDetails")
    }
  }
  ret
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
GetLocationDescriptionList <- function(LocationIdentifier = NULL, ..., .format = TRUE, .perform = TRUE){

  ret <- aquarius(...,
    LocationIdentifier = LocationIdentifier,
    operation = "GetLocationDescriptionList"
  )
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret, query = "/LocationDescriptions")
    }
  }
  ret
}
#' @rdname location-requests
#' @export
GetLocationData <- function(LocationIdentifier, ..., .format = TRUE, .perform = TRUE){
  ret <- aquarius(
    LocationIdentifier = LocationIdentifier,
    operation = "GetLocationData",
    class = "locationdata"
  )
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
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
GetFieldVisitDescriptionList <- function(...,
    LocationIdentifier = NULL,
    QueryFrom = NULL,
    QueryTo = NULL,
    .format = TRUE, .perform = TRUE
){
  ret <- aquarius(...,
    LocationIdentifier = LocationIdentifier,
    QueryFrom = QueryFrom,
    QueryTo = QueryTo,
    operation = "GetFieldVisitDescriptionList"
  )
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret, query = "/FieldVisitDescriptions")
    }
  }
  ret
}

#' @rdname fieldvisit-requests
#' @export
GetFieldVisitData <- function(FieldVisitIdentifier, ..., .format = TRUE, .perform = TRUE){
  ret <- aquarius(...,
    FieldVisitIdentifier = FieldVisitIdentifier,
    operation = "GetFieldVisitData",
    class = "fvdata"
  )
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
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
){
  if(!is.null(Activities)) {
    Activities <- rlang::arg_match(Activities, c("Reading", "Inspection", "CalibrationCheck"))
  }
  ret <- aquarius(
    ...,
    LocationIdentifier = LocationIdentifier,
    Parameters = Parameters,
    Activities = Activities,
    operation = "GetFieldVisitDataByLocation",
    class = "fvdata"
  )
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret, "/FieldVisitData")
    }
  }
  ret
}

#' @rdname fieldvisit-requests
#' @export
GetFieldVisitReadingsByLocation <- function(
    LocationIdentifier,
    LocationUniqueId,
    Parameters,
    ...,
    .format = TRUE,
    .perform = TRUE){
  args <- rlang::list2(...,
    LocationIdentifier = rlang::maybe_missing(LocationIdentifier),
    LocationUniqueId = rlang::maybe_missing(LocationUniqueId),
    Parameters = rlang::maybe_missing(Parameters)
  )
  args <- Filter(Negate(rlang::is_missing), args)
  ret <- aquarius("GetFieldVisitReadingsByLocation") |>
    req_url_query(!!!args)
  if (.perform) {
    ret <- new_aqts_response(httr2::req_perform(ret))
    if (.format){
      ret <- format_response(ret, "/FieldVisitReadings")
    }
  }
  ret
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
GetTimeSeriesUniqueIdList <- function(LocationIdentifier = NULL, Parameter = NULL, ..., .format = TRUE, .perform = TRUE){
  ret <- aquarius(...,
    LocationIdentifier = LocationIdentifier,
    Parameter = Parameter,
    operation = "GetTimeSeriesUniqueIdList"
  )
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

#' @rdname ts-requests
#' @export
GetTimeSeriesDescriptionList <- function(LocationIdentifer = NULL, Parameter = NULL, ..., .format = TRUE, .perform = TRUE){
  ret <- aquarius(...,
    LocationIdentifier = LocationIdentifer,
    Parameter = Parameter,
    operation = "GetTimeSeriesDescriptionList",
    class = "tslist"
  )
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

#' @rdname ts-requests
#' @export
 GetTimeSeriesDescriptionListByUniqueId <- function(TimeSeriesUniqueIds, .format = TRUE, .perform = TRUE){
  ret <- aquarius(
    operation = "GetTimeSeriesDescriptionListByUniqueId",
    class = "tslist"
  ) |>
    req_body_json(data = list(TimeSeriesUniqueIds = TimeSeriesUniqueIds))
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret, "/TimeSeriesDescriptions")
    }
  }
  ret
}

#make_endpoint_function(
#   endpoint = "GetTimeSeriesDescriptionListByUniqueId",
#   node = "TimeSeriesDescriptions",
#   args = rlang::pairlist2(
#     TimeSeriesUniqueIds = rlang::missing_arg(),
#     .method = "POST"
#   )
# )

#' @rdname ts-requests
#' @export
GetTimeSeriesData <- function(TimeSeriesUniqueIds, QueryFrom = NULL, QueryTo = NULL, ..., .format = TRUE, .perform = TRUE){
  ret <- aquarius(...,
    TimeSeriesUniqueIds = TimeSeriesUniqueIds,
    QueryFrom = QueryFrom,
    QueryTo = QueryTo,
    operation = "GetTimeSeriesData",
    class = "tsdata",
    .multi = "explode"

  )
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

#' @rdname ts-requests
#' @export
GetApprovalsTransactionList <- function(TimeSeriesUniqueId = NULL, QueryFrom, QueryTo, ..., .format = TRUE, .perform = TRUE){
  ret <- aquarius(...,
    TimeSeriesUniqueId = TimeSeriesUniqueId,
    QueryFrom = QueryFrom,
    QueryTo   = QueryTo,
    operation = "GetApprovalsTransactionList"
  )
  if (.perform) {
    ret <- req_perform_aqts(ret)
    if (.format){
      ret <- format_response(ret, "/ApprovalsTransactions")
    }
  }
  ret
}

#' @rdname ts-requests
#' @export
GetTimeSeriesCorrectedData <- function(
    TimeSeriesUniqueId,
    QueryFrom,
    QueryTo,
    Unit,
    ...,
    .format = TRUE,
    .perform = TRUE
){
  args <- rlang::list2(...,
    TimeSeriesUniqueId = TimeSeriesUniqueId,
    QueryFrom = rlang::maybe_missing(QueryFrom),
    QueryTo = rlang::maybe_missing(QueryTo),
    Unit = rlang::maybe_missing(Unit)
  )
  args <- Filter(Negate(rlang::is_missing), args)
  args <- tibble::as_tibble(args)
  do_request <- function(x) {
    aquarius("GetTimeSeriesCorrectedData") |> req_url_query(!!!x)
  }
  ret <- args |>
    dplyr::mutate(.id = dplyr::row_number()) |>
    nest(.by = .id) |>
    dplyr::mutate(
      request = map(
        .x = data,
        .f = do_request
      )
    )
  if (.perform) {
    ret <- httr2::req_perform_parallel(
      ret$request,
      pool = curl::new_pool(total_con = 100, host_con = 20)
    )
    ret <- map(ret, new_aqts_response)
    if (.format){
      ret <- map(ret, format_response)
      ret <- dplyr::bind_rows(ret)
    }
  }
  ret
}

#' @rdname ts-requests
#' @export
GetMetadataChangeTransactionList <- function(
    TimeSeriesUniqueId,
    ...,
    .perform = TRUE,
    .format = TRUE
) {
  args <- rlang::list2(...,
    TimeSeriesUniqueId = TimeSeriesUniqueId
  )
  ret <- aquarius("GetMetadataChangeTransactionList") |>
    req_url_query(!!!args)
  if (.perform) {
    ret <- new_aqts_response(httr2::req_perform(ret))
    if (.format){
      ret <- format_response(ret, "/MetadataChangeTransactions")
    }
  }
  ret
}

