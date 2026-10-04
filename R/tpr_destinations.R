#' Get destinations
#'
#' @description Get a list of supported destinations available on 'https://api.themeparks.wiki/'.
#'
#' @importFrom httr modify_url GET content stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom purrr flatten
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr bind_rows any_of glimpse
#'
#' @return a tibble
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
  parsed = tpr_fetch(path, "get destinations")
  parsed = parsed %>%
    purrr::flatten() %>%
    dplyr::bind_rows() %>%
    tidyr::unnest_wider(dplyr::any_of("parks"), names_sep = "_")

  return(parsed)
}
