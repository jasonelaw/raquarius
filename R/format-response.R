#' @export
format_response <- function(x, ...) {
  UseMethod("format_response")
}

drop_status <- function(x) {
  if (rlang::has_name(x, "ResponseStatus")) {
    x[c("ResponseStatus")] <- NULL
  }
  x
}

coerce_timestamps <- function(x) {
  time_probes <- c("timestamp", "StartOf", "EndOf", "LastUpdates")
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::contains(time_probes), parse_timestamp)
    )
}

rename_wp <- function(x) {
  x |>
    dplyr::rename_with(snakecase::to_snake_case) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.list),
        \(x) setNames(x, snakecase::to_snake_case(names(x)))
      ),
      dplyr::across(
        dplyr::where(is.data.frame),
        \(x) dplyr::rename_with(x)
      )
    )
}

drop_one_platform <- function(x) {
  dplyr::select(x, -dplyr::contains("OnePlatform"))
}
#' @export
simd_parse <- function(x, ...) {
  UseMethod("simd_parse")
}

#' @export
simd_parse.httr2_response <- function(x, ...) {
  RcppSimdJson::fparse(httr2::resp_body_raw(x), ...)
}

#' @export
simd_parse.list <- function(x, ...) {
  is_httr_resp <- all(map_lgl(x, \(x) inherits(x, "httr2_response")))
  if (is_httr_resp) {
    x <- map(x, httr2::resp_body_raw)
  }
  is_raw <- all(map_lgl(x, rlang::is_raw))
  if (is_raw) {
    x <- RcppSimdJson::fparse(x, ...)
  } else {
    rlang::abort("`x` must be a list of httr2_response objects or raw vectors")
  }
  x
}

filter_null <- function(x) {
  if (length(x) == 0 || !is.list(x))
    return(x)
  x[!unlist(lapply(x, is.null))]
}

#' Helper function for simplifying common pattern in AQTS json responses
format_row <- function(x) {
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
      dplyr::across(.cols = dplyr::any_of(fields), .fns = parse_timestamp)
    )
}

unnest_wider_namevalue <- function(x, col, value_col = "value", name_col = "name") {
  f <- function(x, name_col, value_col){
    has_value <- rlang::has_name(x, value_col)
    if(has_value){
      tidyr::deframe(x[, c(name_col, value_col)])
    } else {
      list(NULL)
    }
  }
  x |>
    dplyr::mutate(
      "{{ col }}" := f({{ col }},
      name_col = name_col,
      value_col = value_col)
    ) |>
    tidyr::unnest_wider(col = {{ col }})
}

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
  format_row(resp_body_aqts(x)) |>
    mutate(
      ExtendedAttributes <- format_extattr(extendedAttributes, "Value", "Name")
    )
    tidyr::unnest_wider(ExtendedAttributes)
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
format_response.tsdatacorr <- function(x, ...) {
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
format_response.wp_response <- function(x, query,  ...) {
  ret <- tibble(
    response = simd_parse(x$.response, query = query, ...),
    .req_id = x$.req_id
  ) |>
    tidyr::unnest(response) |>
    drop_one_platform()
  ret
}

## Filters -----------------------------------------
#' @export
format_response.filter <- function(x) {
  format_response.wp_response(x, "/filters")
}


## Locations -----------------------------------------
#' @export
format_response.wplocation <- function(x, multiple = FALSE) {
  ret <- tibble(
    location =  simd_parse(x$.response, query = "/location", max_simplify_lvl = 0L),
    .req_id = x$.req_id
  ) |>
    tidyr::unnest_wider(location) |>
    tidyr::unnest_wider(elevationUnit, names_sep = "_") |>
    unnest_wider_namevalue(extendedAttributes, "value", "name")
  type.convert(ret, as.is = TRUE)
}

#' @export
format_response.wplocations <- function(x, multiple = FALSE) {
  ret <- tibble(
    location = simd_parse(x$.response, query = "/locations", max_simplify_lvl = 0L),
    .req_id = x$.req_id
  )  |>
    tidyr::unnest(location) |>
    tidyr::unnest_wider(elevationUnit, names_sep = "_")|>
    unnest_wider_namevalue(extendedAttributes, "value", "name")
  type.convert(ret, as.is = TRUE)
}

## Dataset -----------------------------------------
#' @export
format_response.dataset <- function(x) {
  format_response.wp_response(x, query = "/datasets")
}

#' @export
format_response.lateststatdef <- function(x) {
  format_response.wp_response(x, query = "/latestStatistics") |>
    tidyr::unnest_wider(unit, names_sep = "_")
}

#' @export
format_response.lateststatistic <- function(x) {
  format_response.wp_response(
    x,
    query = "/latestStatisticValues"
  ) |>
    tidyr::hoist(
      statistic,
      statistic_id = "id",
      parameter = "parameter",
      unit = c("unit", "symbol")
    ) |>
    dplyr::select(-statistic)
}

#'@export
format_response.geojson <- function(x) {
  fmt_resp <- function(x, id) {
    sf::st_read(
      dsn = httr2::resp_body_string(x),
      as_tibble = TRUE,
      quiet = TRUE
    ) |>
      coerce_timestamps() |>
      dplyr::bind_cols(.req_id = id)
  }
  dplyr::bind_rows(map2(x$.response, x$.req_id, fmt_resp))
}

#' @export
format_response.export <- function(x) {
  new_tibble(
    tibble(
      export = simd_parse(x$.response),
      .req_id = x$.req_id
    )
  ) |>
    tidyr::unnest_wider(export) |>
    tidyr::unnest_wider(dataset) |>
    tidyr::unnest_wider(timeRange) |>
    convert_time(c("startTime", "endTime")) |>
    dplyr::mutate(
      points = map(
        points,
        \(x) tibble::as_tibble(x) |>
          convert_time(c("timestamp", "eventTimestamp"))
      )
    ) |> unnest(points)
}


#' @export
format_response.bulk <- function(x) {
  ret <- format_response.wp_response(x, "/series")
  ret <- ret |>
    tidyr::unnest_wider(dataset) |>
    tidyr::unnest_wider(timeRange) |>
    dplyr::mutate(
      points = map_if(
        points,
        \(x) !is.null(x),
        \(x) convert_time(x, "timestamp")
      )
    ) |>
    convert_time(c("startTime", "endTime"))
  ret
}

#' @export
format_response.aligned <- function(x) {
  pointer <- c(
    datasets = "/datasets",
    startTime = "/timeRange/startTime",
    endTime = "/timeRange/endTime",
    rows = "/rows"
  )
  ret <- simd_parse(x$response, query = pointer)
  ret <- tibble(aligned = ret, .req_id = x$.req_id) |>
    tidyr::unnest_wider(aligned) |>
    convert_time(c("startTime", "endTime"))
  rows <- purrr::list_rbind(ret$rows) |>
    tidyr::unnest(points)
  ret |>
    dplyr::select(-rows) |>
    tidyr::unnest(datasets) |>
    dplyr::left_join(
      y = rows,
      by = dplyr::join_by(identifier == dataset)
    ) |>
    nest(points = !one_of(c("identifier", "parameter", "label", "unit", "locationIdentifier", "startTime", "endTime")))
}


