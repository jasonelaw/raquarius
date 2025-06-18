test_that("skipping works", {
  skip_if_aqts_offline()
  expect_equal(2*2, 4)
})
