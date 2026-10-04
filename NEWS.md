# themeparkr 0.2.0

## Breaking changes

* `tpr_entity_live()`: `lastUpdated` is now a UTC `POSIXct` instead of a
  character string, and `queue`, `forecast` and `showtimes` hold `NULL`
  (not `NA`) for entities without that data.

* `tpr_entity_children()` returns `latitude` before `longitude`, matching
  `tpr_entity()`.

* `tpr_entity()` only keeps `latitude` and `longitude` from the API's
  `location` field.

* Requires R >= 4.1.

## Bug fixes

* `tpr_entity_children()` no longer errors when a child has null
  coordinates (affected 21 of 161 destinations); those rows get `NA`.

* `tpr_entity_children()` returns a zero-row tibble with the usual columns
  for entities that have no children, instead of erroring.

* `tpr_entity()` no longer errors for entities without a `location` field.

## Other changes

* `tpr_entity_children(park)` is deprecated in favour of
  `tpr_entity_children(id)`, matching the other functions.

* Requests now use httr2: they send a themeparkr user agent, time out after
  30 seconds, and retry when the API is busy. HTTP errors name the function
  you called, include the API's own message, and have class
  `themeparkr_http_error`.

* Removed the unused `print.themeparks_api()` method.

# themeparkr 0.1.3

* Initial GitHub release.
