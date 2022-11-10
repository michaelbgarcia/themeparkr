#' Get live entity details
#'
#' @description Get live data (queue times, parade times, etc.) as well as all
#'     child entities on 'https://api.themeparks.wiki/'.
#'
#' @param park GUID or slug string for the entity of interest from `tpr_destinations()`
#' @importFrom httr modify_url GET content
#' @importFrom purrr pluck map_chr map
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate relocate everything
#' @importFrom glue glue
#' @importFrom tibble tibble
#'
#' @return an object of class `themeparks_api`
#'
#' @details
#' This is recursive, so a destination will
#'    return detais for all parks and all rides within those parks.
#'
#' @examples
#' purrr::map_dfr(tpr_destinations()$content$id[[1]],
#'     ~tpr_entity_live(.x) %>% purrr::pluck("content"))
#'
#'
#'
#' @export
tpr_entity_live = function(park) {
  path = glue::glue("v1/entity/{park}/live")
  url = httr::modify_url("https://api.themeparks.wiki", path = path)
  resp = httr::GET(url)
  parsed = jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  parsed = parsed %>%
    purrr::pluck("liveData")
  parsed = parsed %>%
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
      lastUpparseded = map_chr(parsed, pluck, "lastUpparseded", .default = NA_character_)
    ) %>%
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
