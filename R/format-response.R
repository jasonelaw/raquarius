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

#'
#' fast_tbljson.list <- function(response, query = NULL, ...) {
#'   foo <- function(response) {
#'     RcppSimdJson::fparse(
#'       json = resp_body_raw(response),
#'       query = query,
#'       max_simplify_lvl = 3L
#'     )
#'   }
#'   json <- map(response, foo)
#'   ids <- data.frame(document.id = seq_along(json))
#'   tbl_json(ids, json)
#' }
  foo <- function(response, query = NULL) {
    ret <- RcppSimdJson::fparse(
      json = resp_body_raw(response),
      query = query,
      max_simplify_lvl = 3L
    )
    list(ret)
  }
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

# format_extattr <- function(json) {
#   ext_attr <- json |>
#     tidyjson::enter_object("extendedAttributes")
#
#     ext_attr <- suppressWarnings(tidyjson::gather_array(ext_attr))
#     ext_attr <- ext_attr |>
#     tidyjson::spread_values(name = jstring("name"), value = jstring("value")) |>
#     tidyr::pivot_wider(id_cols = c(document.id, array.index), names_from = name, values_from = value)
#   as_tibble(ext_attr)
# }
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
format_response.aqts_response <- function(x, json_key = NULL, ...) {
  ret <- resp_body_aqts(x, query = json_key)
  # if(!missing(json_key)) {
  #   ret <- ret[[json_key]]
  # }
  as_tibble(ret)
}

#' @export
format_response.GetLocationDataResponse <- function(x, ...) {
  resp <- format_row(resp_body_aqts(x))
    resp <- resp |>
      dplyr::mutate(
        ExtendedAttributes = purrr::map_if(
          .x = ExtendedAttributes,
          .p = ~ !rlang::has_name(., "Value"),
          .f = \(x) dplyr::mutate(x, Value = NA)
        ),
        ExtendedAttributes = purrr::map(
          .x = ExtendedAttributes,
          .f = ~ tidyr::pivot_wider(.,
            id_cols = c(),
            names_from = "Name",
            values_from = "Value"
          )
        )
      ) |>
        tidyr::unnest(ExtendedAttributes)
  resp
}

#' @export
format_response.GetFieldVisitDataResponse <- function(x, ...) {
  resp <- format_row(resp_body_aqts(x)) |>
    dplyr::mutate(
      ApprovalLevel = Approval[[1]]$ApprovalLevel,
      ApprovalLevelDescription = Approval[[1]]$LevelDescription
    ) |>
    dplyr::select(-Approval, -DischargeActivities, -CrossSectionSurveyActivity) |>
    dplyr::mutate(InspectionActivity = map(InspectionActivity, format_row)) |>
    tidyr::unnest(InspectionActivity)
  resp
}

#' @export
format_response.GetTimeSeriesDataResponse <- function(x, ...) {
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
format_response.wp_response <- function(x, query, ...) {
  ret <- resp_body_wp(x, query = query, ...)
  ret <- dplyr::bind_rows(map(ret, format_row))
  ret
}

#' @export
format_response.location <- function(x) {
  ret <- resp_body_wp(x, query = "/location")
  ret <- format_row(ret)
  ret |>
    dplyr::mutate(
      extendedAttributes = map(extendedAttributes, dplyr::bind_rows),
      extendedAttributes = purrr::map_if(
        .x = extendedAttributes,
        .p = ~ !rlang::has_name(., "value"),
        .f = \(x) dplyr::mutate(x, value = NA)
      ),
      extendedAttributes = purrr::map(
        .x = extendedAttributes,
        .f = ~ tidyr::pivot_wider(.,
          id_cols = c(),
          names_from = "name",
          values_from = "value"
        )
      )
    ) |>
    tidyr::hoist(elevationUnit, elevationUnitSymbol = "symbol") |>
    dplyr::select(-elevationUnit) |>
    tidyr::unnest(extendedAttributes)
}

#' @export
format_response.locations <- function(x) {
  ret <- resp_body_wp(x, query = "/locations", max_simplify_lvl = 0L)
  ret <- ret |>
    dplyr::mutate(
      extendedAttributes = map(
        .x = extendedAttributes,
        .f = \(x) setNames(as.list(x$value), x$name))
    ) |>
    tidyr::unnest_wider(extendedAttributes) |>
    tidyr::hoist(elevationUnit, elevationUnitSymbol = "symbol") |>
    dplyr::select(-elevationUnit)

  #
  # ret <- dplyr::bind_rows(map(ret, format_row))
  # fix_value <- function(x) {
  #   ret <- format_row(x)
  #   if(rlang::has_name(ret, name = "value")) {
  #     ret$value <- as.character(ret$value)
  #   }
  #   ret
  # }
  # ret$extendedAttributes <- map(
  #   .x = ret$extendedAttributes,
  #   .f = \(x) dplyr::bind_rows(map(x, fix_value))
  # )
  # eattr <- tidyr::pivot_wider(
  #   data = dplyr::bind_rows(ret$extendedAttributes, .id = "row"),
  #   id_cols = "row",
  #   names_from = "name",
  #   values_from = "value"
  # )
  # ret <- dplyr::bind_cols(ret, eattr)
  # ret <- ret |>
  #    tidyr::hoist(elevationUnit, elevationUnitSymbol = "symbol") |>
  #    dplyr::select(-elevationUnit)
  type.convert(ret, as.is = TRUE)
}

# format_response.location <- function(x) {
#   json <- x |>
#     httr2::resp_body_string() |>
#     tidyjson::enter_object(location)
#   format_location(json)
# }
#
# format_response.locations <- function(x) {
#   json <- x |>
#     httr2::resp_body_string() |>
#     tidyjson::enter_object(locations) |>
#     tidyjson::gather_array()
#   format_location(json)
# }


# format_response.dataset <- function(x) {
#   fast_tbljson(x) |>
#     enter_object("datasets") |>
#     gather_array() |>
#     spread_all() |>
#     as_tibble()
# }

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
  ret <- convert_time(ret, c("startTime", "endTime"))
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

