#' Get entity details
#'
#' @description Get the full data document for a given entity from 'https://api.themeparks.wiki/'.
#'     You can supply either a GUID or slug string.
#'
#' @param id GUID or slug string for the entity of interest
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble_row
#' @importFrom glue glue
#'
#' @return a tibble
#'
#' @details
#' This provides both the response and parsed results from the `/entity/{entityID}` path.
#'
#' @examples
#' park_dest = tpr_destinations()$id[[1]]
#' tpr_entity(park_dest)
#'
#'
#'
#' @export
tpr_entity = function(id) {
  path = glue::glue("v1/entity/{id}")
  parsed = tpr_fetch(path, "get details")
  parsed = c(parsed[names(parsed) != "location"], tpr_location(parsed$location))
  parsed = parsed %>%
    purrr::modify_if(is.list, list) %>%
    tibble::as_tibble()

  return(parsed)
}
