#' Get entity children details
#'
#' @description Get a list of all the children that belong an entity on
#'     'https://api.themeparks.wiki/'.
#'
#' @param park GUID or slug string for the entity of interest
#' @importFrom httr modify_url GET content stop_for_status
#' @importFrom purrr pluck map_dfr modify_at transpose
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr any_of bind_rows mutate relocate everything filter
#' @importFrom tidyr unnest_longer pivot_wider
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
  parsed = parsed %>%
    purrr::pluck("children") %>%
    purrr::map(.f = function(x) {
      purrr::modify_at(x, "location", purrr::transpose)
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(park = park) %>%
    dplyr::relocate(park, .before = dplyr::everything()) %>%
    tidyr::unnest_longer(location,values_to = "location",indices_to = "location_id") %>%
    dplyr::filter(location_id %in% c("latitude","longitude")) %>%
    dplyr::mutate(location = as.numeric(location)) %>%
    tidyr::pivot_wider(names_from = "location_id", values_from = "location")

  return(parsed)
}
