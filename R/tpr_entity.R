#' Get entity details
#'
#' @description Get the full data document for a given entity from 'https://api.themeparks.wiki/'.
#'     You can supply either a GUID or slug string.
#'
#' @param id GUID or slug string for the entity of interest
#' @importFrom httr modify_url GET content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble_row
#' @importFrom glue glue
#'
#' @return an object of class `themeparks_api`
#'
#' @details
#' This provides both the response and parsed results from the `/entity/{entityID}` path.
#'
#' @examples
#' park_dest = tpr_destinations()
#' tpr_entity(park_dest$content$id[[1]])
#'
#'
#'
#' @export
tpr_entity = function(id) {
  path = glue::glue("v1/entity/{id}")
  url = httr::modify_url("https://api.themeparks.wiki", path = path)
  resp = httr::GET(url)
  parsed = jsonlite::fromJSON(httr::content(resp, "text"),
                              simplifyVector = FALSE)
  parsed = parsed %>%
    purrr::modify_if(is.list, list) %>%
    tibble::as_tibble_row()

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "themeparks_api"
  )
}
