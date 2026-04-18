Sys.setenv(AQUARIUS_URL = "https://beswebtst2/AQUARIUS")
Sys.setenv(AQUARIUS_USER = "api-admin")
Sys.setenv(AQUARIUS_PW = "SmileVigorousDiamond")
# PostTag <- function(tag, applicability = c("AppliesToLocations")) {
#   applicability <- match.arg(applicability)
#   req <- aquarius("tags", "provisioning", "POST")  |>
#     req_body_json(list(Key = tag, Applicability = applicability))
#   req_perform(req)
# }
#
# tags <- readr::read_csv(clipboard())
# resps <- map(tags$project_name[-1], PostTag)
#
# #req <- PostTag("Alder Basin Flow Mon")
# #resp <- req_perform(req)
#
# # Connect examples -------------------------------------------------------------
# Sys.setenv(AQUARIUS_CONNECT_URL = "https://beswebtst2/AQConnect/")
# Sys.setenv(AQUARIUS_CONNECT_USER = "jlaw")
# Sys.setenv(AQUARIUS_CONNECT_PW = "Dql0O^6YxaSz$*JBwsFO")
#
#
# response <- connectLogin()
# {
#   connectHealth()
#   connectDbHealth()
#   connectSettingsHealth()
# }
#
# #locs <- data.frame(Identifier = 1:5, Name = letters[1:5], Description = "", UtcOffset = "-8:00")
#
# TriggerOnce(Sys.time())
# TriggerDaily(step = hms::hms(hours = 1))
# sch <- Schedule(
#   name = "Daily",
#   TriggerDaily(step = hms::hms(hours = 1)),
#   TriggerOnce(at = Sys.time())
#   )
#
# tmp <- request("https://www.portlandmaps.com/bes/aquarius/api/v1/locations") |>
#   req_url_query(Active = "true") |>
#   req_perform()
#
# tmp |>
#   enter_object(locations) |>
#   gather_array() |>
#   spread_all()
# tmp4 <- tmp |>
#   enter_object(locations) |>
#   gather_array() |>
#   enter_object("extendedAttributes") |>
#   gather_array()
#
# tmp5 <- tmp4 |>
#   spread_values(name = jstring("name"), value = jstring("value")) |>
#   pivot_wider(id_cols = c(document.id, array.index), names_from = name, values_from = value)
#
#
# ds <- GetMapDataDatasetsByParameter(parameter = "Flow")
# ds <- ds |>
#   filter(dataSetEndOfRecord > ymd("2025-01-01"))
#
# fd <- GetExportBulk(
#   Datasets = Datasets(Identifier = ds$dataSetIdentifier),
#   StartTime = "2025-01-01",
#   EndTime = "2025-01-07T23:59"
# )
# fd <- fd |>
#   unnest(points) |>
#   filter(!is.na(value)) |>
#   mutate(
#     locationIdentifier = forcats::fct_reorder(locationIdentifier, value, .fun = mean)
#   )
#
# day7_plot <- fd |>
#   ggplot() +
#   geom_line(aes(timestamp, value)) +
#   facet_wrap(vars(locationIdentifier), scales = "free_y")
#
# diurnal_plot <- fd |>
#   ggplot() +
#   geom_line(aes(hms::as_hms(timestamp), value, group = yday(timestamp), color = factor(as.Date(timestamp, tz = "America/Los_angeles")))) +
#   facet_wrap(vars(locationIdentifier), scales = "free_y") +
#   scale_colour_viridis_d("Day of Year")
#
# problem_plot <- fd |>
#   filter(locationIdentifier %in% c("TC-7", "TRY-26")) |>
#   ggplot() +
#   geom_line(aes(timestamp, value)) +
#   facet_wrap(vars(locationIdentifier), scales = "free_y")
#
#
# # Arg constructor
# arg_constructor <- function(f) {
#   ret <- function() {
#     argg <- c(as.list(environment()), list(...))
#     tibble(!!!args)
#   }
#   fmls <- rlang::fn_fmls(f)
#   rlang::fn_fmls(ret) <- setdiff(fmls,
#   ret
# }
#
#
#
# library(raquarius)
#
# # Sys.setenv("AQUARIUS_URL" = "URL")
# # Sys.setenv("AQUARIUS_USER" = "USERNAME")
# # Sys.setenv("AQUARIUS_PW" = "PASSWORD")
#
# locs <- GetLocationDescriptionList()
# ids  <- GetTimeSeriesUniqueIdList(LocationIdentifier = locs$Identifier[1])
# ts   <- GetTimeSeriesData(TimeSeriesUniqueIds = ids$UniqueId[1:10])
#
# Sys.setenv("AQUARIUS_WEBPORTAL_URL"  = "URL")
# Sys.setenv("AQUARIUS_WEBPORTAL_USER" = "USERNAME")
# Sys.setenv("AQUARIUS_WEBPORTAL_PW"   = "PASSWORD")
#
# locs  <- GetMapDataAllLocations()
# dsets <- GetMapDataDatasetsByParameter("Temperature")
# ts    <- GetExportDataSet(DataSet = "Precip Increm.1Hour@HYDRA-160", DateRange = "Days30")
#
# (DS <- Datasets(dsets$dataSetIdentifier[1:5], Calculation = "Instantaneous"))
# resp <- GetExportTimeAligned(Datasets = DS, Interval = "PointsAsRecorded", DateRange = "Years1")
#
# GetExportBulk()
#
#
# format_nice <- function(x, ...) {
#   gr = units:::units_options("group")
#   num <- prettyunits::compute_num(drop_units(x))
#   u = paste0(gr[1], num$prefix, units(x), gr[2])
#   x <- drop_units(x)
#   setNames(paste(num$amount, u), names(x))
# }
#
#
# # Delete Reports -------------------------------------------------------------
#
# rep$CreatedTime <- ymd_hms(rep$CreatedTime)
# rep <- GetReportList()
#
# old_reps <- rep |>
#   filter(CreatedTime < (Sys.time() - ddays(30))) |>
#   count(Title) |>
#   arrange(Title)
#
# new_reps <- rep |>
#   filter(CreatedTime > (Sys.time() - ddays(7))) |>
#   count(Title)
#
# reps_to_delete <- old_reps |>
#   anti_join(new_reps, by = "Title") |>
#   pull(Title)
#
# repdel <- map(reps_to_delete, \(x) GetReportList(ReportTitle = x))
# repdel <- list_rbind(repdel)
# ret <- map(repdel$ReportUniqueId, DeleteReportAttachment)
# errors <- map_lgl(ret, httr2::resp_is_error)
# success <- all(!errors)
#
#
# rep <- GetReportList()
#
# reps_to_delete <- rep |>
#   group_by(Title) |>
#   filter(
#      str_starts(Title, "Rainfall Daily Totals"),
#      CreatedTime < max(CreatedTime)
#   ) |>
#   pull(ReportUniqueId)
# ret <- req_perform_parallel(
#   reqs = map(
#     .x = reps_to_delete,
#     .f = \(x) DeleteReportAttachment(x, .perform = FALSE)
#   ),
#   on_error = "continue",
#   progress = TRUE
# )
# errors <- map_lgl(ret, httr2::resp_is_error)
# (success <- all(!errors))
#
# library(rapiclient)
# api_url <- "https://beswebtst2/AQUARIUS/Publish/v2/openapi.json"
#
# pub <- rapiclient::get_api(url = api_url)
# operations <- rapiclient::get_operations(pub)
# schemas <- rapiclient::get_schemas(pub)
