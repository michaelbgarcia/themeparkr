#' Get entity children details
#'
#' @description Get a list of all the children that belong an entity on
#'     'https://api.themeparks.wiki/'.
#'
#' @param park GUID or slug string for the entity of interest
#' @importFrom purrr pluck map_dfr
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr any_of bind_rows mutate relocate everything
#' @importFrom glue glue
#'
#' @return a tibble
#'
#' @details
#' This is recursive, so a destination will
#'    return all parks and all rides within those parks.
#'
#' @examples
#' park_dest = tpr_destinations()$id[[1]]
#' tpr_entity_children(park_dest)
#'
#'
#'
#' @export
tpr_entity_children = function(park) {
  path = glue::glue("v1/entity/{park}/children")
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
    dplyr::mutate(park = park) %>%
    dplyr::relocate(park, .before = dplyr::everything())

  return(parsed)
}
