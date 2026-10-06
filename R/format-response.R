#' @export
format_response <- function(x, ...) {
  UseMethod("format_response")
}

# Helpers ----------------------------------------------------------------------
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
  if (length(x) == 0 || !is.list(x)) {
    return(x)
  }
  x[!unlist(lapply(x, is.null))]
}


convert_time <- function(x, fields) {
  x |>
    dplyr::mutate(
      dplyr::across(.cols = dplyr::any_of(fields), .fns = parse_timestamp)
    )
}

unnest_wider_namevalue <- function(
  x,
  col,
  value_col = "value",
  name_col = "name"
) {
  f <- function(x, name_col, value_col) {
    has_value <- rlang::has_name(x, value_col)
    if (has_value) {
      x[[value_col]] <- as.character(x[[value_col]])
    } else {
      #list(NULL)
      x[[value_col]] <- NA_character_
    }
    tibble::deframe(x[, c(name_col, value_col)])
  }
  ret <- x |>
    dplyr::mutate(
      "{{ col }}" := purrr::map(
        {{ col }},
        \(x) f(x, name_col = name_col, value_col = value_col)
      )
    ) |>
    tidyr::unnest_wider(col = {{ col }})
  type.convert(ret, as.is = TRUE)
}

# Aquarius Responses -----------------------------------------------------------
#' @export
format_response.aqts_response <- function(x, ...) {
  ret <- tibble::tibble(
    response = simd_parse(x, ...)
  ) |>
    tidyr::unnest_wider(response)
  ret
}

#' @export
format_response.locationdata <- function(x, ...) {
  ret <- tibble::tibble(
    response = list(simd_parse(x, ...))
  ) |>
    tidyr::unnest_wider(response)
  if (hasName(ret, "ExtendedAttributes")) {
    ret <- ret |>
      unnest_wider_namevalue(ExtendedAttributes, "Value", "Name")
  }
  ret
}

#' @export
format_response.fvdata <- function(x, ...) {
  resp <- resp_body_aqts(x, query = "/FieldVisitData") |>
    tidyr::hoist(
      Approval,
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
  x <- tibble(response = resp_body_aqts(x)) |>
    pivot_wider(response)
  if (identical(x$Points, list())) {
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
  format_points <- function(x, ...) {
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
