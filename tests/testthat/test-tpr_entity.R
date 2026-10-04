test_that("tpr_entity() returns one row with numeric coordinates", {
  local_fixture("entity-ride")
  out = tpr_entity("924a3b2c-6b4b-49e5-99d3-e9dc3f2e8a48")

  expect_equal(nrow(out), 1)
  expect_equal(out$name, "The Barnstormer")
  expect_type(out$latitude, "double")
  expect_type(out$longitude, "double")
})

test_that("tpr_entity() returns NA coordinates when location is missing", {
  local_fixture("entity-no-location")
  out = tpr_entity("ac44c594-ccfe-41af-9361-304c268c6da4")

  expect_equal(nrow(out), 1)
  expect_identical(out$latitude, NA_real_)
  expect_identical(out$longitude, NA_real_)
})
