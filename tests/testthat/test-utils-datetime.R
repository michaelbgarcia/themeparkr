test_that("tpr_datetime() parses Z and offset timestamps to UTC", {
  out = tpr_datetime(c(
    "2026-10-04T14:23:11.123Z",
    "2026-10-04T10:23:11-04:00",
    "2026-10-04T14:23:11Z"
  ))

  expect_s3_class(out, "POSIXct")
  expect_equal(attr(out, "tzone"), "UTC")
  expect_equal(format(out, "%Y-%m-%d %H:%M:%S"), rep("2026-10-04 14:23:11", 3))
  expect_equal(as.numeric(out[1]) %% 1, 0.123, tolerance = 1e-6)
})

test_that("tpr_datetime() returns NA for missing or unparseable values", {
  expect_identical(is.na(tpr_datetime(c(NA, "not a date"))), c(TRUE, TRUE))
})
