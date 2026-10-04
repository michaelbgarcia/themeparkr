test_that("tpr_destinations() returns one row per park", {
  local_fixture("destinations")
  out = tpr_destinations()

  expect_named(out, c("id", "name", "slug", "externalId", "parks_id", "parks_name"))
  expect_equal(nrow(out), 3)
  expect_equal(
    out$parks_name[out$slug == "sixflags_destination_SFSL"],
    c("Mid-America Parks", "Hurricane Harbor")
  )
})
