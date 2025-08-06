test_that("GetLocation returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- GetLocation(location = "HYDRA-160", .perform = FALSE, .format = FALSE)
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "location")

  expect_error(
    GetLocation(.perform = FALSE, .format = FALSE),
    regexp = "argument \"location\" is missing"
  )

  response <- GetLocation(location = "HYDRA-160", .perform = TRUE, .format = FALSE)
  expect_s3_class(response, "httr2_response")
  expect_identical(httr2::resp_status(response), 200L)

  result <- GetLocation(location = "HYDRA-160", .perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("GetLocations returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- GetLocations(.perform = FALSE, .format = FALSE)
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "locations")

  response <- GetLocations(.perform = TRUE, .format = FALSE)
  expect_s3_class(response, "httr2_response")
  expect_identical(httr2::resp_status(response), 200L)

  result <- GetLocations(.perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("GetDataset returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- GetDataset(.perform = FALSE, .format = FALSE)
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "dataset")

  response <- GetDataset(.perform = TRUE, .format = FALSE)
  expect_s3_class(response, "httr2_response")
  expect_identical(httr2::resp_status(response), 200L)

  result <- GetDataset(.perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("GetMapDataAllLocations returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- GetMapDataAllLocations(.perform = FALSE, .format = FALSE)
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "geojson")

  response <- GetMapDataAllLocations(.perform = TRUE, .format = FALSE)
  expect_s3_class(response, "httr2_response")
  expect_identical(httr2::resp_status(response), 200L)

  result <- GetMapDataAllLocations(.perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "sf")
})

test_that("GetMapDataDatasetsByParameter returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- GetMapDataDatasetsByParameter(parameter = "Precip Increm", .perform = FALSE, .format = FALSE)
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "geojson")

  expect_error(
    GetMapDataDatasetsByParameter(.perform = FALSE, .format = FALSE),
    regexp = "argument \"parameter\" is missing"
  )

  response <- GetMapDataDatasetsByParameter(parameter = "Precip Increm", .perform = TRUE, .format = FALSE)
  expect_s3_class(response, "httr2_response")
  expect_identical(httr2::resp_status(response), 200L)

  result <- GetMapDataDatasetsByParameter(parameter = "Precip Increm", .perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "sf")
})

test_that("GetMapDataLatestStatistics returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- GetMapDataLatestStatistics(parameter = "Precip Increm", statistic = "CALENDAR_1DAY", .perform = FALSE, .format = FALSE)
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "geojson")

  expect_error(
    GetMapDataLatestStatistics(.perform = FALSE, .format = FALSE),
    regexp = "argument \"parameter\" is missing"
  )
  expect_error(
    GetMapDataLatestStatistics(parameter = "Precip Increm", .perform = FALSE, .format = FALSE),
    regexp = "argument \"statistic\" is missing"
  )

  response <- GetMapDataLatestStatistics(parameter = "Precip Increm", statistic = "CALENDAR_1DAY", .perform = TRUE, .format = FALSE)
  expect_s3_class(response, "httr2_response")
  expect_identical(httr2::resp_status(response), 200L)

  result <- GetMapDataLatestStatistics(parameter = "Precip Increm", statistic = "CALENDAR_1DAY", .perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "sf")
})



test_that("GetMapDataPeriodicStatistics returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- GetMapDataPeriodicStatistics(
    parameter = "Precip Increm",
    statistic = "CALENDAR_TOTALS",
    interval = "Daily",
    date = "2025-01-01",
    .perform = FALSE, .format = FALSE
  )
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "geojson")

  expect_error(
    GetMapDataPeriodicStatistics(.perform = FALSE, .format = FALSE),
    regexp = "argument \"parameter\" is missing"
  )
  expect_error(
    GetMapDataPeriodicStatistics(parameter = "Precip Increm", .perform = FALSE, .format = FALSE),
    regexp = "argument \"statistic\" is missing"
  )
  expect_error(
    GetMapDataPeriodicStatistics(
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
  expect_s3_class(response, "httr2_response")
  expect_identical(httr2::resp_status(response), 200L)

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

test_that("GetExportDataSet returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- GetExportDataSet(dataset = "Precip Increm.Primary@HYDRA-160", .perform = FALSE, .format = FALSE)
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "export")

  expect_error(
    GetExportDataSet(
      .perform = FALSE, .format = FALSE
    ),
    regexp = "argument \"dataset\" is missing"
  )

  response <- GetExportDataSet(dataset = "Precip Increm.Primary@HYDRA-160", .perform = TRUE, .format = FALSE)
  expect_s3_class(response, "httr2_response")
  expect_identical(httr2::resp_status(response), 200L)

  result <- GetExportDataSet(dataset = "Precip Increm.Primary@HYDRA-160", .perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})


test_that("GetExportBulk returns the right objects and completes successfully", {
  skip_if_webportal_offline()

  request <- GetExportBulk(
    Datasets = Datasets(
      Identifier = c("Precip Increm.Primary@HYDRA-1", "Precip Increm.Primary@HYDRA-160")
    ),
    .perform = F, .format = F
  )
  expect_s3_class(request, "httr2_request")
  expect_s3_class(request, "wp_request")
  expect_s3_class(request, "bulk")


  response <- GetExportBulk(
    Datasets = Datasets(
      Identifier = c("Precip Increm.Primary@HYDRA-1", "Precip Increm.Primary@HYDRA-160")
    ),
    .perform = T, .format = F
  )
  expect_s3_class(response, "httr2_response")
  expect_identical(httr2::resp_status(response), 200L)

  result <- GetExportBulk(
    Datasets = Datasets(
      Identifier = c("Precip Increm.Primary@HYDRA-1", "Precip Increm.Primary@HYDRA-160")
    ),
    .perform = T, .format = T
  )
  expect_s3_class(result, "data.frame")
})
