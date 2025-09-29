#' @export
format_response <- function(x, ...) {
  UseMethod("format_response")
}

#' @export
fast_tbljson <- function (response, query = NULL, ...)
{
  if (inherits(response, "httr2_response")) {
    response <- list(response)
  }
  json <- map(
    .x = response,
    .f = \(x) RcppSimdJson::fparse(resp_body_raw(x), query = query, max_simplify_lvl = 3L)
  )
  ids <- data.frame(document.id = seq_along(json))
  tbl_json(ids, json)
}

#
# fast_tbljson.list <- function(response, query = NULL, ...) {
#   foo <- function(response) {
#     RcppSimdJson::fparse(
#       json = resp_body_raw(response),
#       query = query,
#       max_simplify_lvl = 3L
#     )
#   }
#   json <- map(response, foo)
#   ids <- data.frame(document.id = seq_along(json))
#   tbl_json(ids, json)
# }
  # foo <- function(response, query = NULL) {
  #   ret <- RcppSimdJson::fparse(
  #     json = resp_body_raw(response),
  #     query = query,
  #     max_simplify_lvl = 3L
  #   )
  #   list(ret)
  # }
filter_null <- function(x) {
  if (length(x) == 0 || !is.list(x))
    return(x)
  x[!unlist(lapply(x, is.null))]
}

#' Helper function for simplifying common pattern in AQTS json responses
format_row <- function(x) {
  #ret <- purrr::map_if(x, is.list, \(x) list(x))
  f <- function(x) {
    !rlang::is_atomic(x)
  }
  ret <- filter_null(x)
  ret <- map_if(ret, f, \(x) list(x))
  ret <- tibble::as_tibble_row(ret)
  ret
}

convert_time <- function(x, fields) {
  x |>
    dplyr::mutate(
      dplyr::across(.cols = dplyr::one_of(fields), .fns = parse_timestamp)
    )
}

format_extattr <- function(x, value_col = "Value", name_col = "Name") {
  has_attr <- map_lgl(x, ~ hasName(.x, value_col))
  x[has_attr] <- map(x[has_attr], ~ setNames(as.list(.x[[value_col]]), .x[[name_col]]))
  x[!has_attr] <- list(NULL)
  x
}
#
# format_location <- function(json) {
#   ext_attr <- format_extattr(json)
#
#   json |>
#     tidyjson::spread_all() |>
#     tibble::as_tibble() |>
#     dplyr::left_join(
#       y = ext_attr
#     )
# }

#fast_tbljson(response, "/locations")

# Aquarius Responses -----------------------------------------------------------
#' @export
format_response.aqts_response <- function(x, ...) {
  ret <- resp_body_aqts(x, ...)
  as_tibble(ret)
}

#' @export
format_response.parameter <- function(x) {
  resp_body_aqts(x, query = "/Parameters", max_simplify_lvl = 0L)
}

#' @export
format_response.locationdata <- function(x, ...) {
  ret <- format_row(resp_body_aqts(x))
  ret$ExtendedAttributes <- format_extattr(ret$ExtendedAttributes, "Value", "Name")
  ret <- ret |>
    tidyr::unnest_wider(ExtendedAttributes)
  ret
}

#' @export
format_response.fvdata <- function(x, ...) {
  resp <- resp_body_aqts(x, query = "/FieldVisitData") |>
    tidyr::hoist(Approval,
      ApprovalLevel = "ApprovalLevel",
      ApprovalLevelDescription = "LevelDescription"
    ) |>
    dplyr::mutate(InspectionActivity = map(InspectionActivity, format_row)) |>
    tidyr::unnest(InspectionActivity, names_sep = "")
  resp
}

#' @export
format_response.tslist <- function(x, ...) {
  resp <- resp_body_aqts(x, query = "/TimeSeriesDescriptions")
  resp
}


#' @export
format_response.tsdata <- function(x, ...) {
  x <- resp_body_aqts(x)
  if(identical(x$Points, list())) {
    points <- list(data = list(tibble::tibble()))
  } else {
    points <- tidyr::pivot_longer(
      data = x$Points,
      cols = -Timestamp,
      names_to = c(".value", "ts"),
      names_pattern = "([a-zA-Z]+)([0-9]{1,2})"
    ) |>
      dplyr::mutate(
        Timestamp = parse_timestamp(Timestamp)
      ) |>
      dplyr::group_by(ts) |>
      tidyr::nest()
  }
  tibble::tibble(x$TimeSeries, Points = points$data)
}

#'@export
format_response.GetTimeSeriesCorrectedDataResponse <- function(x, ...) {
  format_points <- function(x, ...){
    tibble::as_tibble(x) |>
      dplyr::mutate(
        Timestamp = parse_timestamp(Timestamp)
      )
  }
  ret <- x |>
    resp_body_aqts() |>
    format_row()

  if (has_name(ret, "Points")) {
    ret <- ret |>
    dplyr::mutate(
      Points = purrr::map_if(
        .x = Points,
        .p = rlang::is_empty,
        .f = as_tibble
      ),
      Points = purrr::map_if(
        .x = Points,
        .p = Negate(rlang::is_empty),
        .f = format_points
      )
    )
  }
  return(ret)
}

# Web Portal Responses ---------------------------------------------------------
#' @export
format_response.wp_response <- function(x, query, is_array = TRUE, ...) {
  ret <- resp_body_wp(x, query = query, ...)
  if (is_array) {
    ret <- dplyr::bind_rows(map(ret, format_row))
  } else {
    ret <- format_row(ret)
  }
  ret
}

#' @export
format_response.location <- function(x, multiple = FALSE) {
  ret <- resp_body_wp(x, query = if(multiple) "/locations" else "/location", max_simplify_lvl = 0L)
  if(!multiple) {
    ret <- format_row(ret)
  }
  ret$extendedAttributes <- format_extattr(ret$extendedAttributes, "value", "name")
  ret <- ret |>
    tidyr::unnest_wider(extendedAttributes) |>
    tidyr::hoist(elevationUnit, elevationUnitSymbol = "symbol") |>
    dplyr::select(-elevationUnit)
  type.convert(ret, as.is = TRUE)
}

#' @export
format_response.lateststatistic <- function(x) {
  ret <- format_response.wp_response(x, "/latestStatisticValues")
  ret <- ret |>
    tidyr::hoist(statistic, statistic_id = "id", parameter = "parameter", unit = c("unit", "symbol")) |>
    dplyr::select(-statistic)
  ret
}

#'@export
format_response.geojson <- function(x) {
  locs <- st_read(
    dsn = resp_body_string(x),
    as_tibble = TRUE,
    quiet = TRUE
  )
}

#' @export
format_response.export <- function(x) {

  ts <- RcppSimdJson::fparse(resp_body_raw(x))
  ret <- format_row(c(ts$dataset, ts$timeRange))
  if (has_name(ret, "startTime") & has_name(ret, "endTime")){
    ret <- convert_time(ret, c("startTime", "endTime"))
  }
  ret <- tibble::tibble_row(ret, timeseries = list(tibble::as_tibble(ts$points)))

  ret <- ret |>
    dplyr::mutate(
      timeseries = map(
        .x = timeseries,
        .f = \(x) convert_time(x, c("timestamp", "eventTimestamp"))
      )
    )
  ret
}

#' @export
format_response.bulk <- function(x) {
  ret <- format_response.wp_response(x, "/series")
  ret <- ret |>
    tidyr::unnest_wider(dataset) |>
    tidyr::unnest_wider(timeRange) |>
    dplyr::mutate(
      points = map(
        points,
        \(x) convert_time(dplyr::bind_rows(map(x, format_row)), "timestamp"))
    ) |>
    convert_time(c("startTime", "endTime"))
  ret
}

#' @export
format_response.aligned <- function(x) {
  ret <- format_response.wp_response(x, "/Rows")
  ret <- ret |>
    tidyr::unnest_wider(dataset) |>
    tidyr::unnest_wider(timeRange) |>
    dplyr::mutate(
      points = map(
        points,
        \(x) convert_time(dplyr::bind_rows(map(x, format_row)), "timestamp"))
    ) |>
    convert_time(c("startTime", "endTime"))
  ret
}

#' @export
format_response.filter <- function(x) {
  format_response.wp_response(x, "/filters")
}
