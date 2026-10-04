#' Get entity details
#'
#' @description Get the full data document for a given entity from 'https://api.themeparks.wiki/'.
#'     You can supply either a GUID or slug string.
#'
#' @param id GUID or slug string for the entity of interest
#'
#' @return A one-row tibble with the fields the API provides for the entity
#'   (always `id`, `name`, `entityType`; often `slug`, `timezone`, `parentId`,
#'   `parkId`, `destinationId`, `externalId`, and type-specific fields such as
#'   `attractionType`), plus numeric `latitude` and `longitude` columns that are
#'   `NA` when the API has no coordinates.
#'
#' @examplesIf interactive()
#' tpr_entity("waltdisneyworldresort")
#'
#' @export
tpr_entity = function(id) {
  path = glue::glue("v1/entity/{id}")
  parsed = tpr_fetch(path, "get details")
  parsed = c(parsed[names(parsed) != "location"], tpr_location(parsed$location))
  parsed = parsed |>
    purrr::modify_if(is.list, list) |>
    tibble::as_tibble()

  return(parsed)
}
