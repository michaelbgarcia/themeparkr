#' Get entity children details
#'
#' @description Get a list of all the children that belong an entity on
#'     'https://api.themeparks.wiki/'.
#'
#' @param park GUID or slug string for the entity of interest from `tpr_destinations()`
#' @importFrom httr modify_url GET content
#' @importFrom purrr pluck map_dfr
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr any_of bind_rows mutate relocate everything
#' @importFrom glue glue
#'
#' @return an object of class `themeparks_api`
#'
#' @details
#' This is recursive, so a destination will
#'    return all parks and all rides within those parks.
#'
#' @examples
#' purrr::map_dfr(tpr_destinations()$content$id[[1]],
#'     ~tpr_entity_children(.x) %>% purrr::pluck("content"))
#'
#'
#'
#' @export
tpr_entity_children = function(park) {
  path = glue::glue("v1/entity/{park}/children")
  url = httr::modify_url("https://api.themeparks.wiki", path = path)
  resp = httr::GET(url)
  parsed = jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  parsed = parsed %>%
    purrr::pluck("children") %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(park = park) %>%
    dplyr::relocate(park, .before = dplyr::everything())


  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "themeparks_api"
  )
}
