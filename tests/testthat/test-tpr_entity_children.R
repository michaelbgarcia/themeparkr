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

test_that("tpr_entity_children() returns a typed 0-row tibble when there are no children", {
  local_fixture("children-empty")
  out = tpr_entity_children("924a3b2c-6b4b-49e5-99d3-e9dc3f2e8a48")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
  expect_named(out, c("park", "id", "name", "entityType", "externalId",
                      "parentId", "slug", "latitude", "longitude"))
  expect_type(out$latitude, "double")
  expect_type(out$longitude, "double")
})

test_that("tpr_entity_children() takes `id`", {
  local_fixture("children-null-location")
  out = tpr_entity_children(id = "38ca7997-883b-4ae8-a87c-69a74967d59e")

  expect_equal(unique(out$park), "38ca7997-883b-4ae8-a87c-69a74967d59e")
})

test_that("tpr_entity_children(park) still works but is deprecated", {
  rlang::local_options(lifecycle_verbosity = "warning")
  local_fixture("children-null-location")

  expect_warning(
    out <- tpr_entity_children(park = "38ca7997-883b-4ae8-a87c-69a74967d59e"),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(unique(out$park), "38ca7997-883b-4ae8-a87c-69a74967d59e")
})
