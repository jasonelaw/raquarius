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

#' @export
simd_parse <- function(x, ...) {
  UseMethod("simd_parse")
}

#' @export
simd_parse.httr2_response <- function(x, ...) {
  RcppSimdJson::fparse(httr2::resp_body_raw(x), ...)
}

#' @export
simd_parse.aqts_response <- function(x, ...) {
  map(as.list(x), \(x) simd_parse(x, ...))
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
  # f <- function(x, name_col, value_col) {
  #   has_value <- rlang::has_name(x, value_col)
  #   if (has_value) {
  #     x[[value_col]] <- as.character(x[[value_col]])
  #   } else {
  #     #list(NULL)
  #     x[[value_col]] <- NA_character_
  #   }
  #   tibble::deframe(x[, c(name_col, value_col)])
  # }

  # ret <- x |>
  #   dplyr::mutate(
  #     "{{ col }}" := purrr::map(
  #       {{ col }},
  #       \(x) f(x, name_col = name_col, value_col = value_col)
  #     )
  #   ) |>
  #   tidyr::unnest_wider(col = {{ col }})
  #type.convert(ret, as.is = TRUE)
  x |>
    hoist(ExtendedAttributes, name_col, value_col) |>
    unchop(cols = any_of(c(name_col, value_col))) |>
    pivot_wider(
      names_from = name_col,
      values_from = value_col
    ) |>
    type.convert(as.is = TRUE)
}

# Aquarius Responses -----------------------------------------------------------
#' @export
format_response.aqts_response <- function(
  x,
  method = c("wider", "unnest"),
  ...
) {
  method <- match.arg(method)
  api <- attr(x, "api")
  operation <- attr(x, "operation")

  # Intercept any operations that require special handling - use helper
  # function.
  ret <- simd_parse(x, ...)
  ret <- tibble::tibble(response = ret) |>
    dplyr::mutate(.request_id = names(response))
  if (operation == "get_time_series_data") {
    ret <- fmt_get_time_series_data(ret)
  } else if (operation == "get_time_series_corrected_data") {
    ret <- fmt_get_time_series_corrected_data(ret)
  } else if (operation == "get_metadata_change_transaction_list") {
    ret <- fmt_get_metadata_change_transaction_list(ret)
  } else {
    ret <- tidyr::unnest_wider(ret, response)

    ret <- switch(
      operation,
      get_parameter_list = tidyr::unnest(ret, Parameters),
      get_monitoring_method_list = tidyr::unnest(ret, MonitoringMethods),
      get_unit_list = tidyr::unnest(ret, Units),
      get_approval_list = tidyr::unnest(ret, Approvals),
      get_grade_list = tidyr::unnest(ret, Grades),
      get_tag_list = tidyr::unnest(ret, Tags),
      get_active_meters_and_calibrations = tidyr::unnest(
        ret,
        ActiveMeterDetails
      ),
      get_report_list = tidyr::unnest(ret, Reports),
      get_location_description_list = tidyr::unnest(ret, LocationDescriptions),
      get_field_visit_description_list = tidyr::unnest(
        ret,
        FieldVisitDescriptions
      ),
      get_field_visit_data_by_location = ret |>
        tidyr::unnest(FieldVisitData) |>
        tidyr::unnest_wider(Approval) |>
        tidyr::unnest_wider(InspectionActivity, names_sep = ""),
      get_field_visit_readings_by_location = ret |>
        tidyr::unnest(FieldVisitReadings) |>
        tidyr::unnest_wider(Value) |>
        tidyr::unnest_wider(Approval),
      get_time_series_unique_id_list = ret |>
        tidyr::unnest(TimeSeriesUniqueIds),
      get_time_series_description_list = ret |>
        tidyr::unnest(TimeSeriesDescriptions),
      get_time_series_description_list_by_unique_id = ret |>
        tidyr::unnest(TimeSeriesDescriptions),
      get_approvals_transaction_list = tidyr::unnest(
        ret,
        ApprovalsTransactions
      ),
      ret
    )
  }

  status_vars <- c("ResponseVersion", "ResponseTime", "Summary", ".request_id")
  ret <- ret |>
    dplyr::relocate(
      dplyr::any_of(status_vars),
      .after = dplyr::everything()
    ) |>
    fmt_time()
  ret
}

fmt_get_time_series_data <- function(x) {
  pivot_points <- function(x) {
    if (is.data.frame(x)) {
      x |>
        tidyr::pivot_longer(
          cols = -Timestamp,
          names_to = c(".value", "ts"),
          names_pattern = "([a-zA-Z]+)([0-9]{1,2})"
        ) |>
        dplyr::nest_by(ts, .key = "Points") |>
        dplyr::ungroup() |>
        dplyr::mutate(ts = as.integer(ts))
    } else if (is.null(x)) {
      NULL
    } else {
      stop("Unhandled cell type in list column in fmt_get_time_series_data")
    }
  }
  ret <- x |>
    tidyr::unnest_wider(
      response,
      strict = FALSE,
      simplify = list(
        TimeSeries = FALSE,
        Points = FALSE,
        ResponseVersion = TRUE,
        Summary = TRUE
      )
    )
  ts <-
    list(
      TimeSeries = purrr::list_rbind(
        map(
          setNames(ret$TimeSeries, ret$.request_id),
          \(x) x |> dplyr::mutate(ts = 1:dplyr::n())
        ),
        names_to = ".request_id"
      ),
      Points = purrr::list_rbind(
        map(setNames(ret$Points, ret$.request_id), pivot_points),
        names_to = ".request_id"
      )
    )
  if (nrow(ts$Points) > 0) {
    ts <- dplyr::full_join(
      ts$TimeSeries,
      ts$Points,
      by = c(".request_id", "ts")
    )
  } else {
    ts <- ts$TimeSeries
    ts$Points <- rep(list(NULL), nrow(ts))
  }
  ret <- ts |>
    dplyr::full_join(
      ret |>
        dplyr::select(
          TimeRange,
          NumPoints,
          ResponseVersion,
          ResponseTime,
          Summary,
          .request_id
        ),
      by = ".request_id"
    ) |>
    tibble::as_tibble() # |>
  #tidyr::unnest(Points)
  ret
}

fmt_get_time_series_corrected_data <- function(x, ...) {
  ret <- x |>
    tidyr::unnest_wider(
      response,
      strict = FALSE,
      simplify = list(
        Approvals = FALSE,
        Qualifiers = FALSE,
        Methods = FALSE,
        Grades = FALSE,
        GapTolerances = FALSE,
        InterpolationTypes = FALSE,
        Notes = FALSE,
        Points = FALSE,
        ResponseVersion = TRUE,
        Summary = TRUE
      )
    ) |>
    tidyr::unnest_wider(TimeRange)
  ret
}

fmt_get_metadata_change_transaction_list <- function(x) {
  x |>
    tidyr::unnest_wider(
      response,
      simplify = TRUE,
      strict = FALSE
    ) |>
    tidyr::unnest(MetadataChangeTransactions)
}

#' @export
format_response.locationdata <- function(x, ...) {
  ret <- NextMethod(wrap = TRUE)
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
  1
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

fmt_time <- function(x) {
  time_probe <- c(
    "StartTime",
    "EndTime",
    "ResponseTime",
    "Date",
    "LastModified"
  )
  one_cell <- function(x) {
    dplyr::mutate(
      x,
      dplyr::across(
        dplyr::contains(time_probe),
        \(x) {
          lubridate::ymd_hms(x, tz = Sys.timezone(), quiet = TRUE)
        }
      )
    )
  }

  one_column <- function(x) {
    purrr::map_if(x, is.data.frame, one_cell, .else = identity)
  }

  x[] <- purrr::map_if(x, is.list, one_column, .else = identity)
  one_cell(x)
}
