library(dplyr)
library(tidyr)
library(purrr)
library(rlang)
library(httr)
library(jsonlite)
library(glue)

base_url = "https://api.themeparks.wiki/"

# get destinations
tpr_destinations = function() {
  path = "v1/destinations"
  url = httr::modify_url("https://api.themeparks.wiki", path = path)
  resp = httr::GET(url)
  parsed = jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  parsed = parsed %>%
    purrr::flatten() %>%
    dplyr::bind_rows() %>%
    tidyr::unnest_wider(parks, names_sep = "_")

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "themeparks_api"
  )
}

resp_dest = tpr_destinations()

wdw_dest = resp_dest$content %>%
  filter(slug %in% c("waltdisneyworldresort"))

View(wdw_dest)




# get entity
tpr_entity = function(park) {
  path = glue::glue("v1/entity/{park}")
  url = httr::modify_url("https://api.themeparks.wiki", path = path)
  resp = httr::GET(url)
  parsed = jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  parsed = parsed %>%
    tibble::as_tibble() %>%
    tidyr::nest(location = location)

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "themeparks_api"
  )
}

resp_entity = wdw_dest$parks_id %>%
  map_dfr(~tpr_entity(.x) %>% pluck("content"))


# get entity children
tpr_entity_children = function(park) {
  #browser()
  path = glue::glue("v1/entity/{park}/children")
  url = httr::modify_url("https://api.themeparks.wiki", path = path)
  resp = httr::GET(url)
  parsed = jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
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

resp_entity_children = wdw_dest$parks_id %>%
  map_dfr(~tpr_entity_children(.x) %>% pluck("content"))

#
#
# resp = httr::GET(
#   url = base_url,
#   path = glue("v1/entity/47f90d2c-e191-4239-a466-5892ef59a88b/children")
#   #path = glue("v1/entity/e847a8bd-7d21-432b-a7a1-f483517a22b5/children")
# )
#
# if(!length(content(resp)$children)) cat("No children returned for",
#                                         shQuote(pluck(content(resp), "name")))
#
# content(resp) %>% pluck("children") %>% bind_rows()
#
# # resp = httr::GET(
# #   url = base_url,
# #   path = glue("v1/entity/e847a8bd-7d21-432b-a7a1-f483517a22b5/children")
# # )
# #
# # resp_content = content(resp) %>% {
# #   tibble(
# #
# #   )
# # }

# get entity live
tpr_entity_live = function(park) {
  path = glue::glue("v1/entity/{park}/live")
  url = httr::modify_url("https://api.themeparks.wiki", path = path)
  resp = httr::GET(url)
  parsed = jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  parsed = parsed %>%
    purrr::pluck("liveData") %>% {
      tibble(
        id = map_chr(., pluck, "id", .default = NA_character_),
        name = map_chr(., pluck, "name", .default = NA_character_),
        entityType = map_chr(., pluck, "entityType", .default = NA_character_),
        parkId = map_chr(., pluck, "parkId", .default = NA_character_),
        externalId = map_chr(., pluck, "externalId", .default = NA_character_),
        queue = map(., pluck, "queue", .default = NA),
        status = map_chr(., pluck, "status", .default = NA_character_),
        forecast = map(.,pluck,"forecast", .default = NA),
        showtimes = map(., pluck, "showtimes", .default = NA),
        lastUpdated = map_chr(., pluck, "lastUpdated", .default = NA_character_)
      )
    }%>%
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


resp_entity_live = wdw_dest$parks_id %>%
  map_dfr(~tpr_entity_live(.x) %>% pluck("content"))

pr_c_live = tpr_entity_live("273ddb8d-e7b5-4e34-8657-1113f49262a5")
pr_c_live = tpr_entity_live("924a3b2c-6b4b-49e5-99d3-e9dc3f2e8a48") # barnstormer
pr_c_live = tpr_entity_live("e847a8bd-7d21-432b-a7a1-f483517a22b5") #BOG restaurant
pr_c_live$content %>% glimpse()
pr_c_live$content$queue
# Wait time
pr_c_live$content$queue %>% map_dbl(.f = pluck, "STANDBY", "waitTime", .default = NA_real_)
pr_c_live$content$forecast %>% bind_rows()

# wait times current
pr_c_ent = tpr_entity("273ddb8d-e7b5-4e34-8657-1113f49262a5")
pr_c_ent$content %>% glimpse()

pr_c_ent = tpr_entity("273ddb8d-e7b5-4e34-8657-1113f49262a5")
pr_c_ent$content %>% glimpse()






resp = httr::GET(
  url = base_url,
  #path = glue("v1/entity/75ea578a-adc8-4116-a54d-dccb60765ef9/live")
  path = glue("v1/entity/e957da41-3552-4cf6-b636-5babc5cbc4e5/live")
  # path = glue("v1/entity/e847a8bd-7d21-432b-a7a1-f483517a22b5/live")
)
# content(resp) %>% summary()
# content(resp) %>% pluck("liveData")
# content(resp) %>% pluck("liveData") %>% bind_rows()
# content(resp) %>% bind_rows()
# (content(resp) %>% pluck("liveData"))[[1]]
# content(resp) %>% pluck("liveData") %>% map(.f = ~pluck(.x, "id"))
# (content(resp) %>% pluck("liveData") %>% map_depth(2, .f = pluck))[[20]]
# (content(resp) %>% pluck("liveData") %>% map_depth(2, .f = pluck)) %>% map_dfr(.f = ~.x)

# Need to only focus on rides/attractions
# get all types first
# content(resp) %>% pluck("liveData") %>% summary()
# content(resp) %>% pluck("liveData") %>%
#   map(.f = "entityType")
# content(resp) %>% pluck("liveData") %>%
#   keep(.p = ~.x$entityType %in% c("ATTRACTION")) %>%
#   map(.f = names)
# Which fields exist for entity type
# content(resp) %>% pluck("liveData") %>% {
#   tibble(
#     names = map(., names),
#     lens = map(., length),
#     entityType = map_chr(., "entityType")
#   )
# } %>%
#   unnest(cols = c(names, lens)) %>%
#   count(names)
#   # count(names, entityType)

dat = content(resp) %>% pluck("liveData") %>% {
  #keep(.p = ~.x$entityType %in% c("ATTRACTION")) %>% {
  #keep(.p = ~!is.null(.x$forecast)) %>% {
    tibble(
      id = map_chr(., pluck, "id", .default = NA_character_),
      name = map_chr(., pluck, "name", .default = NA_character_),
      entityType = map_chr(., pluck, "entityType", .default = NA_character_),
      parkId = map_chr(., pluck, "parkId", .default = NA_character_),
      externalId = map_chr(., pluck, "externalId", .default = NA_character_),
      queue = map(., pluck, "queue", .default = NA),
      status = map_chr(., pluck, "status", .default = NA_character_),
      forecast = map(.,pluck,"forecast", .default = NA),
      showtimes = map(., pluck, "showtimes", .default = NA),
      lastUpdated = map_chr(., pluck, "lastUpdated", .default = NA_character_)
    )
  }

dat$forecast
# get entity live
resp = httr::GET(
  url = base_url,
  path = glue("v1/entity/47f90d2c-e191-4239-a466-5892ef59a88b/schedule")
  #path = glue("v1/entity/e957da41-3552-4cf6-b636-5babc5cbc4e5/live")
  # path = glue("v1/entity/e847a8bd-7d21-432b-a7a1-f483517a22b5/live")
)
content(resp) %>% summary()




print.themeparks_api <- function(x, ...) {
  cat("<themeparks ", x$path, ">\n", sep = "")
  dplyr::glimpse(x$content)
  invisible(x)
}
