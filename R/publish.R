GetParameterList <- function() {
  req <- aq_publish_req("GetParameterList")
  req |>
    aquarius_perform("Parameters")
}

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

GetTimeSeriesUniqueIdList <- function(...) {
  req <- aq_publish_req("GetTimeSeriesUniqueIdList", ...)
  req |>
    aquarius_perform("TimeSeriesUniqueIds")
}

GetTimeSeriesDescriptionListByUniqueId <- function(req, TimeSeriesUniqueIds) {
  ids <- paste0("TimeSeriesUniqueIds=",TimeSeriesUniqueIds, collapse = "&")
  req <- aq_publish_req("GetTimeSeriesDescriptionListByUniqueId") |>
    req_method("POST") |>
    req_body_raw(ids, type = "application/x-www-form-urlencoded")
  req |>
    aquarius_perform("TimeSeriesDescriptions")
}

GetTimeSeriesData <- function(req, ...) {
  pars <- list2(...)
  pars <- Filter(\(x) !is.null(x), pars)
  pars <- pars_flatten(pars)
  req <- req |>
    publish() |>
    req_url_path_append("GetTimeSeriesData") |>
    req_url_query(!!!pars)
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
