#' Get entity children details
#'
#' @description Get a list of all the children that belong to an entity on
#'     'https://api.themeparks.wiki/'.
#'
#' @param id GUID or slug string for the entity of interest
#' @param park `r lifecycle::badge("deprecated")` Use `id` instead.
#'
#' @return A tibble with one row per child and columns `park` (the `id`
#'   argument), `id`, `name`, `entityType`, `externalId`, `parentId`, `slug`,
#'   and numeric `latitude` and `longitude` (`NA` when the API has no
#'   coordinates). Entities with no children return zero rows with the same
#'   columns.
#'
#' @details
#' This is recursive, so a destination will
#'    return all parks and all rides within those parks.
#'
#' @examplesIf interactive()
#' tpr_entity_children("waltdisneyworldresort")
#'
#' @export
tpr_entity_children = function(id, park = lifecycle::deprecated()) {
  if (lifecycle::is_present(park)) {
    lifecycle::deprecate_warn("0.2.0", "tpr_entity_children(park)", "tpr_entity_children(id)")
    id = park
  }
  path = glue::glue("v1/entity/{id}/children")
  parsed = tpr_fetch(path, "get list of children")
  children = purrr::pluck(parsed, "children")
  # Leaf entities (e.g. a single ride) have no children
  if (length(children) == 0) {
    return(tibble::tibble(
      park = character(), id = character(), name = character(),
      entityType = character(), externalId = character(),
      parentId = character(), slug = character(),
      latitude = double(), longitude = double()
    ))
  }
  parsed = children %>%
    purrr::map(.f = function(x) {
      c(x[names(x) != "location"], tpr_location(x$location))
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(park = !!id) %>%
    dplyr::relocate(park, .before = dplyr::everything())

  return(parsed)
}
