#' Get entity children details
#'
#' @description Get a list of all the children that belong an entity on
#'     'https://api.themeparks.wiki/'.
#'
#' @param park GUID or slug string for the entity of interest
#' @importFrom httr modify_url GET content stop_for_status
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
  url = httr::modify_url("https://api.themeparks.wiki", path = path)
  resp = httr::GET(url)
  httr::stop_for_status(resp, "get list of children")
  parsed = jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  parsed = parsed %>%
    purrr::pluck("children") %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(park = park) %>%
    dplyr::relocate(park, .before = dplyr::everything())

  return(parsed)
}
