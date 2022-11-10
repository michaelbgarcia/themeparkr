#' Get destinations
#'
#' @description Get a list of supported destinations available on 'https://api.themeparks.wiki/'.
#'
#' @importFrom httr modify_url GET content
#' @importFrom jsonlite fromJSON
#' @importFrom purrr flatten
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr bind_rows any_of
#'
#' @return an object of class `themeparks_api`
#'
#' @details
#' This provides both the response and parsed results from the `/destinations` path.
#'
#' @examples
#' tpr_destinations()
#'
#'
#'
#' @export
tpr_destinations = function() {
  path = "v1/destinations"
  url = httr::modify_url("https://api.themeparks.wiki", path = path)
  resp = httr::GET(url)
  parsed = jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  parsed = parsed %>%
    purrr::flatten() %>%
    dplyr::bind_rows() %>%
    tidyr::unnest_wider(dplyr::any_of("parks"), names_sep = "_")

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "themeparks_api"
  )
}
