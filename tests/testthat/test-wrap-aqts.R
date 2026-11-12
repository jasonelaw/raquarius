test_that("Request object is set up appropriately", {
  req <- aquarius(operation = "GetQualifierList", .perform = FALSE)
  expect_s3_class(req, "aqts_request")
  expect_s3_class(req, "vctrs_list_of")
})

test_that("aquarius() function handles vector arguments", {
  req <- aquarius(
    Applicability = c("AppliesToLocations", "AppliesToReports"),
    operation = "GetTagList",
    .perform = FALSE
  )
  expect_s3_class(req, "aqts_request")
  expect_s3_class(req, "vctrs_list_of")
  expect_identical(vctrs::vec_size(req), 2L)

  resp <- perform_aqts_request(req)
  expect_s3_class(resp, "aqts_response")
  expect_s3_class(resp, "vctrs_list_of")
  expect_equal(
    map_int(resp, httr2::resp_status),
    rep(200L, 2),
    ignore_attr = TRUE
  )
})
