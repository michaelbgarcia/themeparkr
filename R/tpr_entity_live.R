#' Get live entity details
#'
#' @description Get live data (queue times, parade times, etc.) as well as all
#'     child entities on 'https://api.themeparks.wiki/'.
#'
#' @param id GUID or slug string for the entity of interest
#'
#' @return A tibble with one row per entity that has live data:
#'   \describe{
#'     \item{id}{Entity GUID.}
#'     \item{status}{e.g. `"OPERATING"`, `"CLOSED"`.}
#'     \item{queue, forecast, showtimes}{List-columns holding the nested API
#'       data, or `NULL` when the entity has none. For example, the standby wait
#'       is `queue[[i]]$STANDBY$waitTime`.}
#'     \item{lastUpdated}{When the API last updated the entry, as a UTC `POSIXct`.}
#'   }
#'
#' @details
#' This is recursive, so a destination will
#'    return details for all parks and all rides within those parks.
#'
#' @examplesIf interactive()
#' tpr_entity_live("waltdisneyworldresort")
#'
#' @export
tpr_entity_live = function(id) {
  path = glue::glue("v1/entity/{id}/live")
  parsed = tpr_fetch(path, "get live data")
  parsed = parsed %>%
    purrr::pluck("liveData")
  parsed =
    tibble::tibble(
      id = purrr::map_chr(parsed, purrr::pluck, "id", .default = NA_character_),
      queue = purrr::map(parsed, purrr::pluck, "queue"),
      status = purrr::map_chr(parsed, purrr::pluck, "status", .default = NA_character_),
      forecast = purrr::map(parsed, purrr::pluck, "forecast"),
      showtimes = purrr::map(parsed, purrr::pluck, "showtimes"),
      lastUpdated = tpr_datetime(
        purrr::map_chr(parsed, purrr::pluck, "lastUpdated", .default = NA_character_)
      )
    )

  return(parsed)
}
