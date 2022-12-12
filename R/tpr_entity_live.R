#' Get live entity details
#'
#' @description Get live data (queue times, parade times, etc.) as well as all
#'     child entities on 'https://api.themeparks.wiki/'.
#'
#' @param id GUID or slug string for the entity of interest
#' @importFrom httr modify_url GET content stop_for_status
#' @importFrom purrr pluck map_chr map
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @importFrom tibble tibble
#'
#' @return a tibble
#'
#' @details
#' This is recursive, so a destination will
#'    return detais for all parks and all rides within those parks.
#'
#' @examples
#' park_dest = tpr_destinations()$id[[1]]
#' tpr_entity_live(park_dest)
#'
#'
#'
#' @export
tpr_entity_live = function(id) {
  path = glue::glue("v1/entity/{id}/live")
  url = httr::modify_url("https://api.themeparks.wiki", path = path)
  resp = httr::GET(url)
  httr::stop_for_status(resp, "get live data")
  parsed = jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  parsed = parsed %>%
    purrr::pluck("liveData")
  parsed =
    tibble(
      id = map_chr(parsed, pluck, "id", .default = NA_character_),
      name = map_chr(parsed, pluck, "name", .default = NA_character_),
      entityType = map_chr(parsed, pluck, "entityType", .default = NA_character_),
      parkId = map_chr(parsed, pluck, "parkId", .default = NA_character_),
      externalId = map_chr(parsed, pluck, "externalId", .default = NA_character_),
      queue = map(parsed, pluck, "queue", .default = NA),
      status = map_chr(parsed, pluck, "status", .default = NA_character_),
      forecast = map(parsed,pluck,"forecast", .default = NA),
      showtimes = map(parsed, pluck, "showtimes", .default = NA),
      lastUpdated = map_chr(parsed, pluck, "lastUpdated", .default = NA_character_)
    )

  return(parsed)
}
