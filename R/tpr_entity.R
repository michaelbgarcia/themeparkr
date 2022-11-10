#' Get entity details
#'
#' @description Get the full data document for a given entity from 'https://api.themeparks.wiki/'.
#'     You can supply either a GUID or slug string.
#'
#' @param park GUID or slug string for the entity of interest from `tpr_destinations()`
#' @importFrom httr modify_url GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr any_of
#' @importFrom tidyr nest
#' @importFrom tibble as_tibble
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
tpr_entity = function(park) {
  path = glue::glue("v1/entity/{park}")
  url = httr::modify_url("https://api.themeparks.wiki", path = path)
  resp = httr::GET(url)
  parsed = jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  parsed = parsed %>%
    tibble::as_tibble() %>%
    tidyr::nest(location = any_of("location"))

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "themeparks_api"
  )
}
