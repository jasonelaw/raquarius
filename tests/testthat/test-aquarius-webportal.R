# get_location --------------------------------------------
test_that("`get_location` ")
test_that("get_location returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- get_location(
    location = "HYDRA-160",
    .perform = FALSE, .format = FALSE
  )
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "wplocation")

  expect_error(
    get_location(.perform = FALSE, .format = FALSE),
    regexp = "`location` is absent but must"
  )

  response <- get_location(
    location = "HYDRA-160",
    .perform = TRUE, .format = FALSE
  )
  expect_s3_class(response, "wp_response")
  expect_identical(resp_status(response), 200L)

  result <- get_location(
    location = "HYDRA-160",
    .perform = TRUE, .format = TRUE
  )
  expect_s3_class(result, "data.frame")
})

test_that("get_locations returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- get_locations(.perform = FALSE, .format = FALSE)
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "wplocations")

  response <- get_locations(.perform = TRUE, .format = FALSE)
  expect_s3_class(response, "wp_response")
  expect_identical(resp_status(response), 200L)

  result <- get_locations(.perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("get_dataset returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- get_dataset(.perform = FALSE, .format = FALSE)
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "dataset")

  response <- get_dataset(.perform = TRUE, .format = FALSE)
  expect_s3_class(response, "wp_response")
  expect_identical(resp_status(response), 200L)

  result <- get_dataset(.perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("get_map_locations returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- get_map_locations(.perform = FALSE, .format = FALSE)
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "geojson")

  response <- get_map_locations(.perform = TRUE, .format = FALSE)
  expect_s3_class(response, "wp_response")
  expect_identical(resp_status(response), 200L)

  result <- get_map_locations(.perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "sf")
})

test_that("get_map_datasets returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- get_map_datasets(
    parameter = "Precip Increm",
    .perform = FALSE, .format = FALSE
  )
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "geojson")

  expect_error(
    get_map_datasets(.perform = FALSE, .format = FALSE),
    regexp = "`parameter` is absent"
  )

  response <- get_map_datasets(
    parameter = "Precip Increm",
    .perform = TRUE, .format = FALSE
  )
  expect_s3_class(response, "wp_response")
  expect_identical(resp_status(response), 200L)

  result <- get_map_datasets(
    parameter = "Precip Increm",
    .perform = TRUE, .format = TRUE
  )
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "sf")
})

test_that("get_map_latest_stat returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- get_map_latest_stat(
    parameter = "Precip Increm",
    statistic = "CALENDAR_1DAY",
    .perform = FALSE, .format = FALSE
  )
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "geojson")

  expect_error(
    get_map_latest_stat(.perform = FALSE, .format = FALSE),
    regexp = "`parameter` is absent"
  )
  expect_error(
    get_map_latest_stat(
      parameter = "Precip Increm",
      .perform = FALSE,
      .format = FALSE
    ),
    regexp = "`statistic` is absent"
  )

  response <- get_map_latest_stat(
    parameter = "Precip Increm",
    statistic = "CALENDAR_1DAY",
    .perform = TRUE,
    .format = FALSE
  )
  expect_s3_class(response, "wp_response")
  expect_identical(resp_status(response), 200L)

  result <- get_map_latest_stat(
    parameter = "Precip Increm",
    statistic = "CALENDAR_1DAY",
    .perform = TRUE,
    .format = TRUE
  )
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "sf")
})



test_that("get_map_periodic_stat returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- get_map_periodic_stat(
    parameter = "Precip Increm",
    statistic = "CALENDAR_TOTALS",
    interval = "Daily",
    date = "2021-01-01",
    .perform = FALSE, .format = FALSE
  )
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "geojson")

  expect_error(
    get_map_periodic_stat(.perform = FALSE, .format = FALSE),
    regexp = "`parameter` is absent"
  )
  expect_error(
    get_map_periodic_stat(parameter = "Precip Increm", .perform = FALSE, .format = FALSE),
    regexp = "`dataset` is absent"
  )
  expect_error(
    get_map_periodic_stat(
      parameter = "Precip Increm",
      statistic = "CALENDAR_TOTALS",
      .perform = FALSE, .format = FALSE
    ),
    regexp = "argument \"interval\" is missing"
  )

  expect_error(
    GetMapDataPeriodicStatistics(
      parameter = "Precip Increm",
      statistic = "CALENDAR_TOTALS",
      interval = "Daily",
      .perform = FALSE, .format = FALSE
    ),
    regexp = "argument \"date\" is missing"
  )


  response <- GetMapDataPeriodicStatistics(
    parameter = "Precip Increm",
    statistic = "CALENDAR_TOTALS",
    interval = "Daily",
    date = "2025-01-01",
    .perform = TRUE, .format = FALSE
  )
  expect_s3_class(response, "wp_response")
  expect_identical(resp_status(response), 200L)

  result <- GetMapDataPeriodicStatistics(
    parameter = "Precip Increm",
    statistic = "CALENDAR_TOTALS",
    interval = "Daily",
    date = "2025-01-01",
    .perform = TRUE, .format = TRUE
  )
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "sf")
})

test_that("export_dataset returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- export_dataset("Precip Increm.Primary@HYDRA-160", .perform = FALSE, .format = FALSE)
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "export")

  expect_error(
    export_dataset(
      .perform = FALSE, .format = FALSE
    ),
    regexp = "`dataset` is absent"
  )

  response <- export_dataset("Precip Increm.Primary@HYDRA-160", .perform = TRUE, .format = FALSE)
  expect_s3_class(response, "httr2_response")
  expect_identical(resp_status(response), 200L)

  result <- export_dataset("Precip Increm.Primary@HYDRA-160", .perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})


test_that("get_export_bulk returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- export_bulk(
    datasets = Datasets(
      Identifier = c("Precip Increm.Primary@HYDRA-1", "Precip Increm.Primary@HYDRA-160")
    ),
    daterange = "Days7",
    .perform = F, .format = F
  )
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "bulk")


  response <- export_bulk(
    datasets = Datasets(
      Identifier = c(
        "Precip Increm.Primary@HYDRA-1", "Precip Increm.Primary@HYDRA-160"
      )
    ),
    daterange = "Days7",
    interval = "PointsAsRecorded",
    .perform = T, .format = F
  )
  expect_s3_class(response, "wp_response")
  expect_identical(resp_status(response), 200L)

  result <- export_bulk(
    Datasets = Datasets(
      Identifier = c("Precip Increm.Primary@HYDRA-1", "Precip Increm.Primary@HYDRA-160")
    ),
    .perform = T, .format = T
  )
  expect_s3_class(result, "data.frame")
})
