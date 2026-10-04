test_that("tpr_entity_live() returns one row per live entity", {
  local_fixture("live-park")
  out = tpr_entity_live("75ea578a-adc8-4116-a54d-dccb60765ef9")

  expect_named(out, c("id", "queue", "status", "forecast", "showtimes", "lastUpdated"))
  expect_equal(nrow(out), 3)
  expect_type(out$queue, "list")
  expect_type(out$forecast, "list")
  expect_type(out$showtimes, "list")
})

test_that("tpr_entity_live() keeps nested live data and uses NULL for gaps", {
  local_fixture("live-park")
  out = tpr_entity_live("75ea578a-adc8-4116-a54d-dccb60765ef9")
  ride = out$id == "0aae716c-af13-4439-b638-d75fb1649df3"
  restaurant = out$id == "55bdcccc-217b-416c-b8b0-4b6a87d16179"

  expect_equal(out$queue[ride][[1]]$STANDBY$waitTime, 5)
  expect_length(out$forecast[ride][[1]], 2)
  expect_null(out$queue[restaurant][[1]])
  expect_null(out$showtimes[ride][[1]])
  expect_null(out$forecast[restaurant][[1]])
})

test_that("tpr_entity_live() parses lastUpdated as UTC date-times", {
  local_fixture("live-park")
  out = tpr_entity_live("75ea578a-adc8-4116-a54d-dccb60765ef9")

  expect_s3_class(out$lastUpdated, "POSIXct")
  expect_equal(attr(out$lastUpdated, "tzone"), "UTC")
  expect_false(anyNA(out$lastUpdated))
})
