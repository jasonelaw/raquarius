# Web Portal -------------------------------------------------------------------
wp_get_url <- make_get_env("AQUARIUS_WEBPORTAL_URL")
wp_get_user <- make_get_env("AQUARIUS_WEBPORTAL_USER")
wp_get_pw <- make_get_env("AQUARIUS_WEBPORTAL_PW")


# Web portal paths to implement
# data-set, filters, latest/{parameter}/{statistic}, statistics/latest, statistics/latest/{parameter},
# statistic-values/latest, statistic-values/latest/{parameter},
# statistic-values/latest/{parameter}/{statistic}, locations, locations/{location},
# map/locations, map/datasets/{parameter}, map/statistics/latest/{parameter}/{statistic},
# map/statistics/periodic/{parameter}/{statistic}/{interval}/{date}
#
webportal <- function(...,
    path = NULL,
    class = NULL,
    url = wp_get_url(),
    username = wp_get_user(),
    password = wp_get_pw(),
    .multi = "explode"
) {
  params <- list2(...)
  verbose <- getOption("raquarius.verbose")
  req <- httr2::request(url) |>
    httr2::req_url_path_append("api", "v1", path) |>
    httr2::req_user_agent("https://github.com/jasonelaw/raquarius") |>
    httr2::req_auth_basic(username, password) |>
    httr2::req_url_query(!!!params, .multi = "explode") |>
    httr2::req_headers(
      `Accept-Encoding` = "gzip"
    )
  if (verbose) {
    req <- httr2::req_verbose(req)
  }
  structure(req, class = c(class, "wp_request", class(req)))
}

resp_body_wp <- function(resp, query = NULL, max_simplify_lvl = 3L) {
  ret <- RcppSimdJson::fparse(resp_body_raw(resp), query = query, max_simplify_lvl = max_simplify_lvl)
  ret[c("ResponseStatus")] <- NULL
  ret
}

webportal_perform <- function(req, extract = NULL, as_tibble = TRUE){
  cli::cli_alert_info(c("Retrieving response from {.url {req$url}}"))
  resp <- req |>
    req_perform() |>
    resp_body_string() |>
    jsonlite::fromJSON()
  cli::cli_alert_success("Finished!")
  #cli::cli_alert_info("{resp$Summary}")
  resp$Status <- NULL
  ret <- resp[[extract]]
  if(as_tibble) {
    ret <- tibble::as_tibble(ret)
  }
  ret
}

new_wp_response <- function(x, class) {
  stopifnot(inherits(x, "httr2_response"))
  #path <- httr2::resp_url_path(x)
  new_class <- c(class, "wp_response", class(x))
  structure(x, class = new_class)
}

req_perform_wp <- function(x, max_active = 10, on_error = "stop") {
  is_request <- inherits(x, "httr2_request")
  if (is_request) {
    return(new_wp_response(httr2::req_perform(x), class = class(x)[1]))
  }
  is_list_of_requests <- is.list(x) &&
    all(sapply(x, \(x) inherits(x, "httr2_request")))
  this_class <- unique(sapply(x, \(x) class(x)[1]))
  stopifnot(is_list_of_requests, identical(length(this_class), 1L))

  response <- req_perform_parallel(x, max_active = max_active, on_error = on_error)
  map(response, \(x) new_wp_response(x, class = this_class))
}

# Web Portal Routes ------------------------------------------------------------
#'
#' See the web portal API documentation for arguments.
#' #' @param ... pass query arguments to route. Please see Web Portal API documentation
#' for available arguments.
#' @name webportal-routes
NULL

# {
#   if (.perform) {
#     ret <- new_aqts_response(httr2::req_perform(ret))
#     if (.format){
#       ret <- format_response(ret, "/LocationDescriptions")
#     }
#   }
# }

## Locations -------------------------------------------------------------------

GetLocation <- function(location, ..., .format = TRUE, .perform = TRUE) {
  location <- URLencode(location)
  ret <- webportal(..., class = "location")  |>
    req_template("locations/{location}")
    if (.perform) {
      ret <- req_perform_wp(ret)
      if (.format){
        ret <- format_response(ret)
      }
    }
  ret
}

GetLocations <- function(..., .format = TRUE, .perform = TRUE) {
  ret <- webportal(..., path = "locations", class = "locations")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

## Dataset ---------------------------------------------------------------------

#' @export
GetDataset <- function(..., .perform = TRUE, .format = TRUE) {
  ret <- webportal(..., path = "data-set", class = "dataset")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret, query = "/datasets")
    }
  }
  ret
}

## Latest Statistics -----------------------------------------------------------

#' @describeIn webportal-routes Get Latest Statistics
#' @export
GetLatestStatisticValues <- function(..., .format = TRUE, .perform = TRUE) {
  ret <- webportal(..., path = "statistic-values/latest", class = "lateststatistic")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

#' @describeIn webportal-routes Get Latest Statistics
#' @export
GetLatestStatisticValuesByParameter <- function(parameter, ..., .format = TRUE, .perform = TRUE) {
  parameter <- URLencode(parameter)
  ret <- webportal(..., class = "lateststatistic") |>
    req_template("statistic-values/latest/{parameter}")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

#' @describeIn webportal-routes Get Latest Statistics
#' @export
GetLatestStatisticValuesByStatistic <- function(parameter, statistic, ..., .format = TRUE, .perform = TRUE) {
  parameter <- URLencode(parameter)
  statistic <- URLencode(statistic)
  ret <- webportal(..., class = "lateststatistic") |>
    req_template("statistic-values/latest/{parameter}/{statistic}")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

## Maps -----------------------------------------------------------------------

#' @describeIn webportal-routes List all locations
#' @export
GetMapDataAllLocations <- function(..., .perform = TRUE, .format = TRUE) {
  ret <- webportal(..., path = c("map", "locations"), class = "geojson")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

#' @describeIn webportal-routes List timeseries by parameter; return sf object
#' @export
GetMapDataDatasetsByParameter <- function(parameter, ..., .perform = TRUE, .format = TRUE) {
  parameter <- URLencode(parameter)
  ret <- webportal(..., class = "geojson") |>
    req_template("map/datasets/{parameter}")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

#' @describeIn webportal-routes Get latest statistics; return sf object
#' @export
GetMapDataLatestStatistics <- function(parameter, statistic, ..., .perform = TRUE, .format = TRUE) {
  parameter <- URLencode(parameter)
  statistic <- URLencode(statistic)
  ret <- webportal(..., class = "geojson") |>
    req_template("/map/statistics/latest/{parameter}/{statistic}")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

#' @describeIn webportal-routes Get periodic statistics; return sf object
#' @export
GetMapDataPeriodicStatistics <- function(parameter, statistic, interval, date, ..., .perform = TRUE, .format = TRUE) {
  parameter <- URLencode(parameter)
  statistic <- URLencode(statistic)
  interval <- URLencode(interval)
  date <- URLencode(date)
  ret <- webportal(..., class = "geojson") |>
    req_template("/map/statistics/periodic/{parameter}/{statistic}/{interval}/{date}")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

## Export ----------------------------------------------------------------------

#' @describeIn webportal-routes Export a data set
#' @export
GetExportDataSet <- function(dataset, ..., .perform = TRUE, .format = TRUE) {
  ret <- webportal(dataset = dataset, ..., path = c("export", "data-set"), class = "export")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

#' @export
GetExportPeriodicStatistic <- function(
    dataset, statistic, calendar, interval, ...,
    .perform = TRUE,
    .format = TRUE
) {
  ret <- webportal(
    dataset = dataset,
    statistic = statistic,
    calendar = calendar,
    interval = interval,
    ...,
    path = c("export", "data-set"),
    class = "export")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

GetExportSeasonalStatistic <- NULL

#' @describeIn webportal-routes Export several time-aligned data sets.
#' @export
GetExportTimeAligned <- function(
    Datasets, Calendar = NULL, StartTime = NULL, EndTime = NULL, Step = NULL, Timezone = NULL,
    DateRange = c(
      "EntirePeriodOfRecord", "OverlappingPeriodOfRecord", "Today",
      "Days7", "Days30", "Months6", "Years1"
    ),
    Interval = c(
      "PointsAsRecorded", "Minutely", "Hourly", "Daily", "Monthly",
      "Yearly"
    ),
    RoundData = FALSE, IncludeGradeCodes = TRUE, IncludeQualifiers = TRUE,
    IncludeApprovalLevels = TRUE, IncludeInterpolationTypes = TRUE, ...) {
  DateRange <- match.arg(DateRange)
  Interval <- match.arg(Interval)
  data <- dots_list(
    Datasets = Datasets,
    Calendar = Calendar,
    StartTime = StartTime,
    EndTime = EndTime,
    Step = Step,
    Timezone = Timezone,
    RoundData = RoundData, DateRange = DateRange, Interval, RoundData, IncludeGradeCodes,
    IncludeQualifiers, IncludeApprovalLevels, IncludeInterpolationTypes, .ignore_empty = "all"
  )
  req <- aq_webportal_req(path = c("export", "time-aligned")) |>
    req_method("POST") |>
    req_body_json(data)
  # resp <- req |> req_perform()
  req
}

#' Read from the Web Portal export tab
#'
#' This function reads from the Web Portal export tab.
#' https://aquarius.portlandoregon.gov/Export/BulkExport?DateRange=EntirePeriodOfRecord&TimeZone=-8&Calendar=CALENDARYEAR&Interval=PointsAsRecorded&Step=1&ExportFormat=csv&TimeAligned=False&RoundData=False&IncludeGradeCodes=True&IncludeApprovalLevels=True&IncludeQualifiers=True&IncludeInterpolationTypes=True&Datasets[0].DatasetName=Temperature.7DADM%40P0337&Datasets[0].Calculation=Instantaneous&Datasets[0].UnitId=168&_=1686947628225
#' @export
GetExport <- function(...) {
  req <- aq_webportal_req(NULL, ...) |>
    req_url_path("Export", "DataSet")
  resp <- req |> req_perform()
  # ret <- resp |>
  #   resp_body_string() |>
  #   jsonlite::fromJSON()
  resp
}

GetExportBulk <- function(..., .perform = TRUE, .format = TRUE) {
  args <- list2(...)
  ret <- webportal(path = c("export", "bulk"), class = "bulk") |>
    req_body_json(data = args)
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}
# ret <- GetExportBulk(
#  Datasets = Datasets(Identifier = c("Precip Increm.Primary@HYDRA-1", "Precip Increm.Primary@HYDRA-160"), Calculation = "Aggregate"),
# Interval = "Daily", DateRange = "Years1", .perform = FALSE)

# Try it out -------------------------------------------------------------------
# Sys.setenv("AQUARIUS_WEBPORTAL_URL"  = "https://aquarius.portlandoregon.gov")
# Sys.setenv("AQUARIUS_WEBPORTAL_USER" = "public")
# Sys.setenv("AQUARIUS_WEBPORTAL_PW"   = "NMyvrdAc&9")
#
# locs <- GetMapDataAllLocations()
# sets <- GetMapDataDatasetsByParameter("Temperature")
# resp <- GetExportDataSet(DataSet = sets$dataSetIdentifier[1])
