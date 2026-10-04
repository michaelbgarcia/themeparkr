# Internal: extract numeric latitude/longitude from an API `location` object.
# Missing or null coordinates become NA so every row keeps the same columns.
tpr_location = function(location) {
  list(
    latitude = as.numeric(purrr::pluck(location, "latitude", .default = NA)),
    longitude = as.numeric(purrr::pluck(location, "longitude", .default = NA))
  )
}
