test_that("tpr_entity_children() keeps children with null coordinates as NA", {
  local_fixture("children-null-location")
  out = tpr_entity_children("38ca7997-883b-4ae8-a87c-69a74967d59e")

  expect_equal(nrow(out), 3)
  expect_type(out$latitude, "double")
  expect_type(out$longitude, "double")

  null_row = out[out$id == "ac44c594-ccfe-41af-9361-304c268c6da4", ]
  expect_identical(null_row$latitude, NA_real_)
  expect_identical(null_row$longitude, NA_real_)
  expect_equal(sum(is.na(out$latitude)), 1)
})
