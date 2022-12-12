#' Get entity details
#'
#' @description Get the full data document for a given entity from 'https://api.themeparks.wiki/'.
#'     You can supply either a GUID or slug string.
#'
#' @param id GUID or slug string for the entity of interest
#' @importFrom httr modify_url GET content stop_for_status
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
  url = httr::modify_url("https://api.themeparks.wiki", path = path)
  resp = httr::GET(url)
  httr::stop_for_status(resp, "get details")
  parsed = jsonlite::fromJSON(httr::content(resp, "text"),
                              simplifyVector = FALSE)
  parsed = parsed %>%
    purrr::modify_if(is.list, list) %>%
    tibble::as_tibble_row()

  return(parsed)
}
