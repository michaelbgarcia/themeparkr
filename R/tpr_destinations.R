#' Get destinations
#'
#' @description Get the list of destinations (resorts) supported by
#'     'https://api.themeparks.wiki/', with the parks that belong to each.
#'
#' @return A tibble with one row per park:
#'   \describe{
#'     \item{id, name, slug, externalId}{The destination.}
#'     \item{parks_id, parks_name}{A park within that destination.}
#'   }
#'
#' @examplesIf interactive()
#' tpr_destinations()
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
