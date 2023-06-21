# Web Portal -------------------------------------------------------------------
# A little bit of httr2 code to call the Aquarius Web Portal API. Scroll to
# bottom of script to see an example - make sure that the username and password
# environment variables are set to your web portal username and password.

## Packages --------------------------------------------------------------------
# library(tidyverse)
# library(httr2)
# library(sf)
# library(tibble)
# library(rlang)

## Functions -------------------------------------------------------------------
# process_pars <- function(...){
#   pars <- list2(...)
#   pars <- Filter(\(x) !is.null(x), pars)
#   pars <- pars_flatten(pars)
#   pars
# }
#
# pars_flatten <- function(x){
#   lx <- sapply(x, length)
#   i  <- lx > 1
#   nms <- names(x)
#   x[i] <- lapply(x[i], as.list)
#   setNames(
#     object = c(x, recursive = TRUE),
#     nm     = rep(nms, lx)
#   )
# }
#
# webportal <- function() {
#   url  <- Sys.getenv("AQUARIUS_WEBPORTAL_URL")
#   user <- Sys.getenv("AQUARIUS_WEBPORTAL_USER")
#   pw   <- Sys.getenv("AQUARIUS_WEBPORTAL_PW")
#   request(url) |>
#     # req_error(
#     #   is_error = \(x) resp_status(x) >= 400#,
#     #   #body     = \(x) resp_body_html(x)
#     # ) |>
#     req_auth_basic(user, pw)
# }
#
# aq_webportal_req <- function(path, ...) {
#   pars <- process_pars(...)
#   webportal() |>
#     req_url_path_append("api", "v1", path) |>
#     req_url_query(!!!pars)
# }

#' Web Portal Routes
#'
#' See the web portal API documentation for arguments.
#' @rdname webportal-routes
NULL

#' @describeIn webportal-routes List all locations
#' @export
GetMapDataAllLocations <- function(...) {
  req <- aq_webportal_req(c("map", "locations"), ...)
  resp <- req |> req_perform()
  locs <- st_read(
    dsn = resp |> resp_body_string(),
    as_tibble = TRUE,
    quiet = TRUE
  )
}

#' @describeIn webportal-routes List timeseries by parameter
#' @export
GetMapDataDatasetsByParameter <- function(parameter, ...) {
  req <- aq_webportal_req(NULL, ...) |>
    req_template("map/datasets/{parameter}")
  resp <- req |> req_perform()
  locs <- st_read(
    dsn = resp |> resp_body_string(),
    as_tibble = TRUE,
    quiet = TRUE
  )
}

#' @describeIn webportal-routes Export a data set
#' @export
GetExportDataSet <- function(...){
  req <- aq_webportal_req(path = c("export", "data-set"), ...)
  resp <- req |> req_perform()
  ret <- resp |>
    resp_body_string() |>
    jsonlite::fromJSON()
  if(ret$numPoints[1] > 0) {
    ret <- ret$points|>
      add_column(!!!ret$dataset, .before = 1)
  }
  ret
}

#' @describeIn webportal-routes Export several time-aligned data sets.
#' @export
GetExportTimeAligned <- function(
    Datasets, Calendar = NULL, StartTime = NULL, EndTime = NULL, Step = NULL, Timezone = NULL,
    DateRange = c("EntirePeriodOfRecord", "OverlappingPeriodOfRecord", "Today",
                  "Days7", "Days30", "Months6", "Years1"),
    Interval  = c("PointsAsRecorded", "Minutely", "Hourly", "Daily", "Monthly",
                  "Yearly"),
    RoundData = FALSE, IncludeGradeCodes = TRUE, IncludeQualifiers = TRUE,
    IncludeApprovalLevels = TRUE, IncludeInterpolationTypes = TRUE, ...){
  DateRange = match.arg(DateRange)
  Interval = match.arg(Interval)
  data <- list2(
    Datasets = Datasets,
    Calendar = Calendar,
    StartTime = StartTime,
    EndTime = EndTime,
    Step = Step,
    Timezone = Timezone,
                RoundData = RoundData, DateRange = DateRange, Interval, RoundData, IncludeGradeCodes,
                IncludeQualifiers, IncludeApprovalLevels, IncludeInterpolationTypes, .ignore_empty = "all")
  req <- aq_webportal_req(path = c("export", "time-aligned")) |>
    req_method("POST") |>
    req_body_json(data)
  # resp <- req |> req_perform()
  req
}

#'Read from the Web Portal export tab
#'
#'This function reads from the Web Portal export tab.
#'https://aquarius.portlandoregon.gov/Export/BulkExport?DateRange=EntirePeriodOfRecord&TimeZone=-8&Calendar=CALENDARYEAR&Interval=PointsAsRecorded&Step=1&ExportFormat=csv&TimeAligned=False&RoundData=False&IncludeGradeCodes=True&IncludeApprovalLevels=True&IncludeQualifiers=True&IncludeInterpolationTypes=True&Datasets[0].DatasetName=Temperature.7DADM%40P0337&Datasets[0].Calculation=Instantaneous&Datasets[0].UnitId=168&_=1686947628225
#' @export
GetExport <- function(...){
  req <- aq_webportal_req(NULL, ...) |>
    req_url_path("Export", "DataSet")
  resp <- req |> req_perform()
  # ret <- resp |>
  #   resp_body_string() |>
  #   jsonlite::fromJSON()
  resp
}

# Try it out -------------------------------------------------------------------
# Sys.setenv("AQUARIUS_WEBPORTAL_URL"  = "https://aquarius.portlandoregon.gov")
# Sys.setenv("AQUARIUS_WEBPORTAL_USER" = "public")
# Sys.setenv("AQUARIUS_WEBPORTAL_PW"   = "NMyvrdAc&9")
#
# locs <- GetMapDataAllLocations()
# sets <- GetMapDataDatasetsByParameter("Temperature")
# resp <- GetExportDataSet(DataSet = sets$dataSetIdentifier[1])
