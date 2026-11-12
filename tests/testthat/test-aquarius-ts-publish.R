test_that("GetParameterList returns the right objects and completes successfully", {
  skip_if_aqts_offline()

  request <- GetParameterList(.perform = FALSE, .format = FALSE)
  expect_s3_class(request, "aqts_request")
  expect_s3_class(request, "vctrs_list_of")

  # expect_error(
  #   GetLocation(.perform = FALSE, .format = FALSE),
  #   regexp = "argument \"location\" is missing"
  # )

  response <- GetParameterList(.perform = TRUE, .format = FALSE)
  expect_s3_class(response, "aqts_response")
  expect_s3_class(response, "vctrs_list_of")
  expect_identical(httr2::resp_status(response[[1]]), 200L)

  result <- GetParameterList(.perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})


test_that("GetMonitoringMethodList returns the right objects and completes successfully", {
  skip_if_aqts_offline()

  request <- GetMonitoringMethodList(.perform = FALSE, .format = FALSE)
  expect_s3_class(request, "aqts_request")
  expect_s3_class(request, "vctrs_list_of")

  response <- GetMonitoringMethodList(
    .perform = TRUE,
    .format = FALSE
  )
  expect_s3_class(response, "aqts_response")
  expect_s3_class(response, "vctrs_list_of")
  expect_identical(httr2::resp_status(response[[1]]), 200L)

  result <- request <- GetMonitoringMethodList(.perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("GetUnitList returns the right objects and completes successfully", {
  skip_if_aqts_offline()

  request <- GetUnitList(.perform = FALSE, .format = FALSE)
  expect_s3_class(request, "aqts_request")
  expect_s3_class(request, "vctrs_list_of")

  response <- request <- GetUnitList(.perform = TRUE, .format = FALSE)
  expect_s3_class(response, "aqts_response")
  expect_s3_class(response, "vctrs_list_of")
  expect_identical(httr2::resp_status(response[[1]]), 200L)

  result <- request <- GetUnitList(.perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("GetApprovalList returns the right objects and completes successfully", {
  skip_if_aqts_offline()

  request <- GetApprovalList(.perform = FALSE, .format = FALSE)
  expect_s3_class(request, "aqts_request")
  expect_s3_class(request, "vctrs_list_of")

  response <- GetApprovalList(.perform = TRUE, .format = FALSE)
  expect_s3_class(response, "aqts_response")
  expect_s3_class(response, "vctrs_list_of")
  expect_identical(httr2::resp_status(response[[1]]), 200L)

  result <- request <- GetApprovalList(.perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("GetGradeList returns the right objects and completes successfully", {
  skip_if_aqts_offline()

  request <- GetGradeList(.perform = FALSE, .format = FALSE)
  expect_s3_class(request, "aqts_request")
  expect_s3_class(request, "vctrs_list_of")

  response <- GetGradeList(.perform = TRUE, .format = FALSE)
  expect_s3_class(response, "aqts_response")
  expect_s3_class(response, "vctrs_list_of")
  expect_identical(httr2::resp_status(response[[1]]), 200L)

  result <- GetApprovalList(.perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("GetQualifierList returns the right objects and completes successfully", {
  skip_if_aqts_offline()

  request <- GetQualifierList(.perform = FALSE, .format = FALSE)
  expect_s3_class(request, "aqts_request")
  expect_s3_class(request, "vctrs_list_of")

  response <- GetQualifierList(.perform = TRUE, .format = FALSE)
  expect_s3_class(response, "aqts_response")
  expect_s3_class(response, "vctrs_list_of")
  expect_identical(httr2::resp_status(response[[1]]), 200L)

  result <- GetQualifierList(.perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("GetTagList returns the right objects and completes successfully", {
  skip_if_aqts_offline()

  request <- GetTagList(.perform = FALSE, .format = FALSE)
  expect_s3_class(request, "aqts_request")
  expect_s3_class(request, "vctrs_list_of")

  response <- GetTagList(.perform = TRUE, .format = FALSE)
  expect_s3_class(response, "aqts_response")
  expect_s3_class(response, "vctrs_list_of")
  expect_identical(httr2::resp_status(response[[1]]), 200L)

  result <- GetTagList(.perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("GetLocationDescriptionList returns the right objects and completes successfully", {
  skip_if_aqts_offline()

  request <- GetLocationDescriptionList(.perform = FALSE, .format = FALSE)
  expect_s3_class(request, "aqts_request")
  expect_s3_class(request, "vctrs_list_of")

  response <- GetLocationDescriptionList(.perform = TRUE, .format = FALSE)
  expect_s3_class(response, "aqts_response")
  expect_s3_class(response, "vctrs_list_of")
  expect_identical(httr2::resp_status(response[[1]]), 200L)

  result <- GetLocationDescriptionList(.perform = TRUE, .format = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("GetLocationData returns the right objects and completes successfully", {
  skip_if_aqts_offline()

  request <- GetLocationData(
    LocationIdentifier = "VNB",
    .perform = FALSE,
    .format = FALSE
  )
  expect_s3_class(request, "aqts_request")
  expect_s3_class(request, "vctrs_list_of")

  response <- GetLocationData(
    LocationIdentifier = "VNB",
    .perform = TRUE,
    .format = FALSE
  )
  expect_s3_class(response, "aqts_response")
  expect_s3_class(response, "vctrs_list_of")
  expect_identical(httr2::resp_status(response[[1]]), 200L)

  result <- GetLocationData(
    LocationIdentifier = "VNB",
    .perform = TRUE,
    .format = TRUE
  )
  expect_s3_class(result, "data.frame")
})

test_that("GetFieldVisitDescriptionList returns the right objects and completes successfully", {
  skip_if_aqts_offline()

  request <- GetFieldVisitDescriptionList(
    LocationIdentifier = "VNB",
    .perform = FALSE,
    .format = FALSE
  )
  expect_s3_class(request, "aqts_request")
  expect_s3_class(request, "vctrs_list_of")

  response <- GetFieldVisitDescriptionList(
    LocationIdentifier = "VNB",
    .perform = TRUE,
    .format = FALSE
  )
  expect_s3_class(response, "aqts_response")
  expect_s3_class(response, "vctrs_list_of")
  expect_identical(httr2::resp_status(response[[1]]), 200L)

  result <- GetFieldVisitDescriptionList(
    LocationIdentifier = "VNB",
    .perform = TRUE,
    .format = TRUE
  )
  expect_s3_class(result, "data.frame")
})


test_that("GetFieldVisitData returns the right objects and completes successfully", {
  skip_if_aqts_offline()

  request <- GetFieldVisitDescriptionList(
    LocationIdentifier = "VNB",
    .perform = FALSE,
    .format = FALSE
  )
  expect_s3_class(request, "aqts_request")
  expect_s3_class(request, "vctrs_list_of")

  response <- GetFieldVisitDescriptionList(
    LocationIdentifier = "VNB",
    .perform = TRUE,
    .format = FALSE
  )
  expect_s3_class(response, "aqts_response")
  expect_s3_class(response, "vctrs_list_of")
  expect_identical(httr2::resp_status(response[[1]]), 200L)

  result <- GetFieldVisitDescriptionList(
    LocationIdentifier = "VNB",
    .perform = TRUE,
    .format = TRUE
  )
  expect_s3_class(result, "data.frame")
})
