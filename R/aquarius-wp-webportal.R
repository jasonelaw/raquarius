
# Web Portal Routes ------------------------------------------------------------
#'Web Portal API
#'
#' See the web portal API documentation for arguments.
#' @param ... pass query arguments to route. Please see Web Portal API
#' documenation for available arguments.
#' @param .perform if `TRUE`, perform the request, otherwise return a
#' `httr2::request` object
#' @param .format if `TRUE`, return a `data.frame`, otherwise return a
#' `httr2::response` object. Ignored if `.perform = FALSE`

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

## Filters -------------------
#' @describeIn webportal-routes Get filters for map endpoints
#' @export
GetFilters <- function(.perform = TRUE, .format = TRUE) {
  ret <- webportal(path = "filters", class = "filter")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}
## Locations -------------------------------------------------------------------

#' @describeIn webportal-routes Get a location
#' @export
GetLocation <- function(location, ..., .format = TRUE, .perform = TRUE) {
  location <- URLencode(location)
  ret <- webportal(..., class = "location")  |>
    req_template("locations/{location}")
    if (.perform) {
      ret <- req_perform_wp(ret)
      if (.format){
        ret <- format_response(ret, multiple = FALSE)
      }
    }
  ret
}

#' @describeIn webportal-routes Get locations
#' @export
GetLocations <- function(..., .format = TRUE, .perform = TRUE) {
  ret <- webportal(..., path = "locations", class = "location")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret, multiple = TRUE)
    }
  }
  ret
}

## Dataset ---------------------------------------------------------------------
#' @describeIn webportal-routes Get datasets
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

## Latest Statistic Definitions ------------------------------------------------

#' @describeIn webportal-routes Get Latest Statistics
#' @export
GetLatestStatistics <- function(parameter = NULL, statistic = NULL, active = NULL, .format = TRUE, .perform = TRUE) {
  ret <- webportal(parameter = parameter, statistic = statistic, active = active,
    path = c("statistics", "latest"), class = "lateststatisticdef"
  )
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret, "/latestStatistics") |>
        tidyr::unnest_wider(unit, names_sep = "")
    }
  }
  ret
}

#' @describeIn webportal-routes Get Latest Statistics
#' @export
GetLatestStatistic <- function(parameter = NULL, statistic = NULL, ..., .format = TRUE, .perform = TRUE) {
  parameter <- URLencode(parameter)
  statistic <- URLencode(statistic)
  ret <- webportal(..., class = "lateststatisticdef") |>
    req_template("statistics/latest/{parameter}/{statistic}")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret, "/latestStatistic", is_array = FALSE) |>
        tidyr::unnest_wider(unit, names_sep = "")
    }
  }
  ret
}

#' @describeIn webportal-routes Get Latest Statistics
#' @export
GetLatestStatisticsByParameter <- function(parameter = NULL, ..., .format = TRUE, .perform = TRUE) {
  parameter <- URLencode(parameter)
  ret <- webportal(..., class = "lateststatisticdef") |>
    req_template("statistics/latest/{parameter}")
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret, "/latestStatistics") |>
        tidyr::unnest_wider(unit, names_sep = "")
    }
  }
  ret
}

## Latest Statistics Values ----------------------------------------------------

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
  parameter <- identity(parameter)
  statistic <- identity(statistic)
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
  parameter <- identity(parameter)
  statistic <- identity(statistic)
  interval  <- identity(interval)
  date      <- identity(date)
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
GetExportDataSet <- function(DataSet, ..., .perform = TRUE, .format = TRUE) {
  ret <- webportal(DataSet = DataSet, ..., path = c("export", "data-set"), class = "export")
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

#' @describeIn webportal-routes Export seasonal statistics
#' @export
GetExportSeasonalStatistic <- function(
    dataset = NULL,
    interval = NULL,
    statistic = NULL,
    referenceperiod = NULL,
    ...,
    .perform = TRUE,
    .format = TRUE
) {
  ret <- webportal(
    dataset = dataset, interval = interval, statistic = statistic,
    referenceperiod = referenceperiod, ...,
    path = c("export", "seasonal-statistic"), class = "export"
  )
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
}

#' @describeIn webportal-routes Export several time-aligned data sets.
#' @export
GetExportTimeAligned <- function(..., .perform = TRUE, .format = TRUE) {
  args <- list2(...)
  ret <- webportal(path = c("export", "time-aligned"), class = "aligned") |>
    #req_method("POST") |>
    req_body_json(args)
  if (.perform) {
    ret <- req_perform_wp(ret)
    if (.format){
      ret <- format_response(ret)
    }
  }
  ret
}

#' Read from the Web Portal export tab
#'
#' This function reads from the Web Portal export tab.
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

#' @describeIn webportal-routes Export datasets in bulk; optionally summarize
#' @export
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
