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
wdw_ids = c("8b5b9a00-b2cc-4bd9-bf39-9c312c17e8cb", "d58db4d0-3bad-4655-937b-1cbc9ed0e880",
            "75ea578a-adc8-4116-a54d-dccb60765ef9", "47f90d2c-e191-4239-a466-5892ef59a88b",
            "b2148542-8777-4bdc-b2c3-f366d80281a1", "5b7cf10a-763b-45ba-9049-87140270826f",
            "e847a8bd-7d21-432b-a7a1-f483517a22b5", "17d51899-7d6d-4cd7-93bb-4064a47d501b",
            "8854deed-264b-4dcd-bf1a-6dd83e023f13", "3b290419-8ca2-44bc-a710-a6c83fca76ec",
            "90ab2c64-e05c-4783-bde5-fddf41f78402", "273ddb8d-e7b5-4e34-8657-1113f49262a5",
            "1a2e70d9-50d5-4140-b69e-799e950f7d18", "924a3b2c-6b4b-49e5-99d3-e9dc3f2e8a48",
            "759e7496-565c-420f-b0bd-777dfd598f08", "6ef1b126-5b0b-46a1-8608-4fcf98ab92c8",
            "42328c39-76ab-4f03-b862-4206c8d9f7bb", "ada6a611-31ef-426a-a1f4-bfce8503f4a6",
            "2bf8f4ac-9b56-46dc-9521-329a8ac2c637", "f61c4451-d0d6-440a-bdc7-cf059b5ffbc3",
            "fdc8e386-4fc9-4d0f-ba0d-d3df695df071", "811984d7-56f4-4ca6-848d-896d46aca846",
            "3d8f8f8f-f984-4d2e-8dea-5a79432bdf05", "ade9aa6d-2eb0-4046-b04d-a0f98d0682d9",
            "66ff36de-9cb3-4d9a-b891-1665d19ffb3e", "a349429a-5ef8-41c4-9a28-84ed4f7a0465",
            "16fdff47-c8b1-4228-b97d-242f3663396e", "d9d12438-d999-4482-894b-8955fdb20ccf",
            "ee070d46-6a64-41c0-9f12-69dcfcca10a0", "f010bc01-b450-4476-a5f3-a5f2813104b2",
            "8d7ccdb1-a22b-4e26-8dc8-65b1938ed5f0", "ec246026-7c77-4bd2-981c-7aaf6f0fc7e5",
            "514be058-91fc-4bf5-8c03-050f148ec765", "d56a99da-8d1c-47c9-bb42-220806da3d48",
            "e40ac396-cbac-43f4-8752-764ed60ccceb", "1897c9b3-436c-4046-937d-b6ff98233177",
            "06c599f9-1ddf-4d47-9157-a992acafc96b", "72c7343a-f7fb-4f66-95df-c91016de7338",
            "101a254c-a268-4984-a149-eb1bde8346f0", "f5aad2d4-a419-4384-bd9a-42f86385c750",
            "796b0a25-c51e-456e-9bb8-50a324e301b3", "890fa430-89c0-4a3f-96c9-11597888005e",
            "96455de6-f4f1-403c-9391-bf8396979149", "7c5e1e02-3a44-4151-9005-44066d5ba1da",
            "0d94ad60-72f0-4551-83a6-ebaecdd89737", "4bef7560-ed81-47c7-b178-6544abe3daaf",
            "2ebfb38c-5cb5-4de1-86c0-f7af14188022", "ffcfeaa2-1416-4920-a1ed-543c1a1695c4",
            "8d255a0c-f0e5-4bac-b63f-f159dbccd101", "888fb4a4-7adf-47a1-8ba2-c258cc64fd75",
            "917783a5-da9c-4c55-ac51-0ac1f8253131", "422e7379-741e-4935-a2f7-6bee7662d81d",
            "2b3e5eeb-0cbc-4d19-a46e-07b5ddf8a700", "329c91db-3b4f-40e5-adaa-f902e6f450f9",
            "64a6915f-a835-4226-ba5c-8389fc4cade3", "8f353879-d6ac-4211-9352-4029efb47c18",
            "1627c724-112c-4361-aeba-dae80082c90d", "81b15dfd-cf6a-466f-be59-3dd65d2a2807",
            "35ed719b-f7f0-488f-8346-4fbf8055d373", "22f48b73-01df-460e-8969-9eb2b4ae836c",
            "18c533d6-a395-4ae4-9488-80fce9c497fe", "8c8cd77d-97f6-4309-b285-42aad90e9f15",
            "07dbeaea-85fa-45f2-872f-02f9e7510419", "83201c33-ce99-45ae-a595-f47e61d73739",
            "d298e9e5-cb62-40f7-803a-5e8500023474", "cf9d12fb-1e06-4c8e-ae6f-1e7f1e5c861c",
            "3e5f26ee-c02d-47fd-891e-5e4479073444", "30fe3c64-af71-4c66-a54b-aa61fd7af177",
            "00666fe9-7774-4b53-9fb7-3d333f8aa503", "6f1d3b25-42c9-4e99-9dce-6c20d7a5deea",
            "480fde8f-fe58-4bfb-b3ab-052a39d4db7c", "d2581db2-025f-4192-b18a-b3dfaee963cd",
            "8183f3f2-1b59-4b9c-b634-6a863bdf8d84", "3a5eb21b-7463-48c0-8167-4e784c2e011b",
            "37bb0aca-0dcf-4fa0-bc03-81d1b4ef9ac0", "4f0df9e7-d4c1-45b5-93e2-4a7bc92547b0",
            "ba44245e-5562-4359-be27-9dfb2d96cb2d", "861bf387-f362-4904-9082-99e093efa6ee",
            "8a877212-9020-43d2-9ead-604b392d8cce", "e943c0e3-4c73-40a5-8501-899ec0b3d047",
            "d51d654d-6fd0-4814-8427-2709c527e0fc", "219f8463-0cb6-4434-80f8-87f289ea0eb9",
            "49dfad5e-0f82-42dd-9ded-5a5d3cac11a6", "849728c7-0043-4bbe-af64-e65679679e09",
            "20b5daa8-e1ea-436f-830c-2d7d18d929b5", "b760c20d-1877-4de1-9f6a-a24f52b06acf",
            "51392ca4-f824-42d8-8808-8110ec8e0e22", "34c4916b-989b-4ff1-a7e3-a6a846a3484f",
            "6e118e37-5002-408d-9d88-0b5d9cdb5d14", "5262e79e-8553-4ec0-a832-3177d377136d",
            "9211adc9-b296-4667-8e97-b40cf76108e4", "76dfa347-94bb-4552-b183-a08490c54acc",
            "6f6998e8-a629-412c-b964-2cb06af8e26b", "288747d1-8b4f-4a64-867e-ea7c9b27bad8",
            "1c84a229-8862-4648-9c71-378ddd2c7693", "d56506e2-6ad3-443a-8065-fea37987248d",
            "b070cbc5-feaa-4b87-a8c1-f94cca037a18", "d2298e86-9dd2-487c-b3f8-8d7ba2404c09",
            "ead53ea5-22e5-4095-9a83-8c29300d7c63", "7bf05bdc-8279-427b-8a6d-50fc62ef31cb",
            "0beccd84-7a6d-454a-94d3-c8a3265e7e8d", "ae1fe649-d98a-4d22-be7b-8555495c808c",
            "8b2bcfc1-23f6-4893-aca8-8225b1884a07", "97552d3f-8ddf-4aa5-b7b1-e14d5bd05568",
            "3655bbe2-8a41-4f35-a99f-e4d9d582c5fa", "4edb7625-3f86-4da6-9a31-c0cd72fa94fc",
            "9fd9c4e3-2fbc-4705-975e-67abe45c4281", "f39dbb08-35db-4ad4-885d-e4787de5bde6",
            "4f391f0e-52be-4f9d-99d6-b3ae0373b43c", "7a5af3b7-9bc1-4962-92d0-3ea9c9ce35f0",
            "239d5e6c-28c4-4ed4-80a9-d332692c0758", "125d9166-634b-41ec-a29c-182bb0a71dba",
            "dd0392bc-2ebb-48ab-b1b5-1af1193acee2", "e7976e25-4322-4587-8ded-fb1d9dcbb83c",
            "380c7bc4-646e-439a-8699-be1cd603a36d", "3b7309a5-7b57-4edf-b4e6-36ad958ac21e",
            "d581fdde-6679-46e5-821d-a33c9b4cc7ed", "904f17dd-0ef3-4794-be7e-58b884bf81b5",
            "d04ec77c-c082-46fd-a253-c7c69bb14374", "fe6cee07-ba72-42c1-abfa-0ce4ef360eb4",
            "a15ce7cf-342a-4c7a-9372-7a1fa1054747", "41b5c2ea-7c6f-47b5-8330-c00ffa347376",
            "4d27b0d7-2b0a-4569-90fa-e79f117ec7ef", "bc1ffa86-9b1a-4ce9-84a5-b479dfa3cb53",
            "b61e138d-d0bd-4b3a-bb2c-7f899d116b9b", "f432be3e-6900-4146-8ed5-c02210e4666e",
            "e12fb6d6-0985-44e5-bdd3-65ff74433063", "4c58f313-25f8-49ca-9820-483de2005c39",
            "cea213d9-bf4f-408a-ac61-768e7709ab7d", "fd30e928-a22b-4d93-930e-64bfb84aabda",
            "7e4402eb-0f0b-41d3-8713-3e57b7b58af8", "babeeb66-7289-4665-ba04-395f40358c8f",
            "1b39b2bc-8bf3-4482-b0cf-9ad7329dc42f", "185c53bc-7153-4bf1-a582-5c03d18c90ee",
            "310d5c7e-433c-46bb-91ee-da8bc39edf32", "59588cd8-2bcd-4908-b5b0-64d2e75f5c04",
            "697473c5-ae17-4c3f-80c7-09a813248013", "8f0f437a-d46e-4582-8289-f65afd619ea1",
            "296d164a-8b9a-49d3-a285-637b86731260", "6fd1e225-53a0-4a80-a577-4bbc9a471075",
            "15928f42-8205-4bd5-bead-4bcd20b7a618", "90d79335-c907-4069-a021-d0fe1ec73ae2",
            "853687d0-5de6-43ec-b0c3-ac43f1217e77", "64085946-a050-4b56-b1d9-61340091f112",
            "0aae716c-af13-4439-b638-d75fb1649df3", "83fed9d1-c25f-497c-ab8e-21e4dd7ce655",
            "0f57cecf-5502-4503-8bc3-ba84d3708ace", "e39b831b-7731-49bb-815b-289b4f49a9fd",
            "4d34dd80-4042-4ada-a74f-ef35a0887b4f", "15107c34-65a3-473c-aaeb-655b508f529d",
            "c5f6c7e4-d5cd-4d71-8e02-57d8bb43b3ba", "1a8d9843-c058-437b-9830-7e25642c0f5f",
            "fb076275-0570-4d62-b2a9-4d6515130fa3", "ea730d0c-7723-429d-89a4-9e76f053f5ac",
            "9244365a-ffad-4ea1-ad8d-a36ac3a0e383", "57acb522-a6fc-4aa4-a80e-21f21f317250",
            "001b099f-4db8-4843-8c1e-852e0e8fc2df", "1c708beb-41e1-43ae-8dd8-1e85075aeb38",
            "9053240b-7f7f-44fe-970b-bd7956cd5d4f", "97011c19-a864-4922-b63b-6519ca9e6ed0",
            "d7669edc-eaa1-4af2-bbb5-6e98df564166", "1eee22e8-1d0a-4809-a42b-df3ae55c69d5",
            "3dc6e90f-de6e-4bf0-bae9-b04bc930351a", "a9f971fc-b5b6-4c6a-9b51-1d71bb1b2d3f",
            "48e7f9f2-ddc3-4ced-8d11-a5cd968085c2", "2ecc4fff-2994-476f-9926-24a4af173838",
            "8f8746cb-c714-4c60-848d-e2dc4e6f586b", "97e0cc07-cf2b-4d28-941a-beff79f21543",
            "0d798096-5d94-477c-8c06-d256432ecaac", "f0437f34-d2fa-4a48-a6cc-d0402f3cd69e",
            "4132b6fd-97e7-48c6-9185-28dc0527c74f", "12a0dc40-e3cd-409d-b82f-ede9e7136a81",
            "94de4db5-70ab-4bd8-b50b-fa9914f1b8e4", "8998c35b-722b-4b0c-9379-95b09c92d774",
            "2d7534b1-2d17-4462-99ee-7ef02f802ded", "f0e0d1a3-b1a1-42a1-b514-df96b7951db2",
            "c600282c-227b-4ffc-b1ee-b5987609ee4f", "ae9a4a79-ccf1-4561-9a95-cf83d3007b98",
            "c9e1d1f6-021f-43f2-a14b-3e67f65adbc4", "c616884f-c55f-4f74-b58d-1a8374717380",
            "c4b64a31-e855-46a9-9d1a-aa13c99e88af", "aa846b60-a845-4e66-979c-ad7ff75b2dbb",
            "20b61c85-1c69-4576-8d8e-ba7b16915577", "29d9849e-db54-4d23-a561-bf2a35ef3a98",
            "7f97be37-1210-462a-8039-1751c9b0b6a8", "dd39f200-6465-42e6-bff1-6a8b25512e6e",
            "4807262a-ad16-4d43-a0d7-64b24603ef36", "a5171a3d-829b-4ec8-ae0b-edf2c2f32309",
            "32e01181-9a5f-4936-8a77-0dace1de836c", "bc997600-fcc0-4f6f-b908-a1419b26cfd8",
            "ff9aa159-0b93-400b-a3ad-1b2a0640c132", "eb6a60d7-b354-47a1-a091-0beb11b24188",
            "12bcd0e4-be02-4e6f-ba9b-a3686fd826fc", "a0c546b1-e915-4d6d-9c93-d013c77aa620",
            "99ab827f-6beb-46ef-8538-8e97bb8f667b", "f819079e-644e-4fce-bda3-26b899ac7027",
            "b90a0a24-dc6b-4d33-a848-ebf8a4862e67", "0f7d52ca-25fe-4eea-ad92-129268351342",
            "6bad073d-3dd9-4500-bb85-29fa06c94ff8", "224d803c-cd7a-45d8-bfab-d843f2030983",
            "55bdcccc-217b-416c-b8b0-4b6a87d16179", "dbf4a977-a7b5-41db-a9b7-ecad65f9aa0f",
            "e14e439d-d1aa-4f3a-90e9-60c524bd0b5a", "38892221-f69c-4913-97c9-7473c88854fa",
            "3206e3a6-cbc3-4960-9700-163764bc47d6", "6ecdbfc8-85c2-436d-893b-6db0f437b74a",
            "43e5558e-2be0-4989-b80b-074afa8302a9", "de3309ca-97d5-4211-bffe-739fed47e92f",
            "9d4d5229-7142-44b6-b4fb-528920969a2c", "37ae57c5-feaf-4e47-8f27-4b385be200f0",
            "f163ddcd-43e1-488d-8276-2381c1db0a39", "a5241f3b-4ab5-4902-b5ba-435132ef553d",
            "5b6475ad-4e9a-4793-b841-501aa382c9c0", "24cf863c-b6ba-4826-a056-0b698989cbf7",
            "d58d9262-ec95-4161-80a0-07ca43b2f5f3", "55c531b6-3ce2-4c47-a8a1-0dc9107d825b",
            "166f2985-7b27-4eff-a8b3-29c3448ba198", "7dec8e5f-e082-4453-8fb3-f2f1da8b8aca",
            "15700490-3749-45cf-a737-3cba56e13704", "13d7c9d1-a785-403d-9ba8-71fee2f02d55",
            "365816dc-e99c-4b55-bafa-ba2cecd3ed96", "0f40274d-420a-425a-9377-29fd6e49484f",
            "86a41273-5f15-4b54-93b6-829f140e5161", "75449e85-c410-4cef-a368-9d2ea5d52b58",
            "b218c3de-0f7f-436c-be44-2f5112d5d139", "17ecfc8c-8013-4d48-805b-f37f94cc2452",
            "b9be5b8f-fe1f-4a77-bcad-5399429ecaa6", "5327448f-463d-4567-8c5d-f1a169f4c14f",
            "1a8ea967-229a-42a0-8290-59b036c84e14", "b5d6d1d1-e960-4c8f-a8a4-b9748b386b64",
            "910bfe49-97f0-4d79-bbd3-5badbade096c", "c35fd0d8-3a15-4609-8edf-92204c20d0e6",
            "d8214ba6-22a7-4a92-8977-bae3779d5c4c", "0164bf43-29ab-4528-8052-98befbe81bcb",
            "b2260923-9315-40fd-9c6b-44dd811dbe64", "399aa0a1-98e2-4d2b-b297-2b451e9665e1",
            "e516f303-e82d-4fd3-8fbf-8e6ab624cf89", "258bca41-e850-4d5a-86ea-3a4d82a59449",
            "161f82e8-eaf8-4401-811b-fe2b45cccd0e", "b9516054-a8b4-4d4c-aa0b-a60f3c3dc360",
            "6f427ef0-9c56-4041-8f78-357ad2fcf390", "db310566-6acc-4edf-9699-2a1f01097664",
            "a3886522-e19f-4154-8cdf-1ca7160e4e2b", "bd408ea5-efba-47da-84cc-e304ac0ee974",
            "896053f3-55e7-40f9-9473-208d4bc43d33", "75d43698-162c-47af-a607-e879637dc8e7",
            "1f542745-cda1-4786-a536-5fff373e5964", "93d3c183-737f-4e26-98f9-b18a3e190393",
            "6a8d3517-dc06-4d1c-bc38-5a84f397c8a9", "03b2f688-ed6c-4804-91e0-6f2333bf2f3f",
            "b1ed574f-40f2-4132-a1ee-893a276633c8", "6225cefe-eada-453b-9443-d1e679e28cda",
            "fc64aa10-290f-472e-9fab-4d649f53ab2d", "d8497541-53a2-4206-b95f-39cb238ae511",
            "697cbc3f-bf13-4e68-b980-c9059a762c0d", "d9dbd260-9e34-406d-abb7-19f9d733e577",
            "b83b6dc9-9573-4e15-ab00-9ed8a2334770", "93843fb0-ee5a-49c6-91a8-40a042e93865",
            "eb63c263-1589-40f2-82fc-c7865eb1feb0", "736b2e6c-daaf-45c4-ba9e-fb60d7cf92d6",
            "d4179c4c-eb08-4559-aa7c-f9802fda641b", "f8f36470-ca60-4072-ba2c-9d186a337cac",
            "2551a77d-023f-4ab1-9a19-8afec0190f39", "7969166f-feef-4350-b26e-6a6c745528f4",
            "61fb49f8-e62f-4e1c-ae0e-8ab9929037bc", "f9e169bf-1e27-47c9-9f0f-ce8c6df4da53",
            "0941f812-f214-4837-9e67-ee1216ceed50", "6fbe6d02-4057-43bb-80a3-047b1e8a50ca",
            "bc2c2ffe-3cf0-4e75-a408-bf5e86515c4c", "7357772c-6b11-4a8d-af97-05a1bb45f001",
            "d90f0556-52f4-41d2-b21f-67d36fafbf75", "af420c20-96c0-4ea2-bb7f-d68385291f88",
            "e3549451-b284-453d-9c31-e3b1207abd79", "9a3553b0-b0de-4f8e-b0a9-a434ae306813",
            "1e735ffb-4868-47f1-b2cd-2ac1156cd5f0", "a86b36a4-a439-43cd-8994-4030a159e261",
            "482169b9-2889-4747-8aef-f9d13a37d940", "3e490fd6-8e1c-44d5-9eef-59ce897402f3",
            "4c41aa4f-556f-40bb-93ff-f0eef4ff2038", "07263f57-0431-4da2-a8b6-77d2965a6f83",
            "3b20cc04-567d-4272-b582-c009ef050424", "ac6c2ad6-c1c1-4f41-824c-8820dd3d0d26",
            "6ea670b8-3097-4d60-b69c-94e16071da17", "f020b5e3-d26d-4339-83f9-0f1f858a0e40",
            "c49ec22b-b529-4d25-bafa-e222b22ddc3c", "bdb71c67-172f-44d5-8e41-d736912d5cde",
            "bb74811e-b80e-4d24-bca1-d880f0af4732", "e348336b-a035-44dc-bf7f-c9ebf0389a9d",
            "e8f0b426-7645-4ea3-8b41-b94ae7091a41", "e8dccc86-cfb4-44c3-9a90-e95b88edbd21",
            "375197ac-27ac-41f7-bd93-f4e9b9fc4d5d", "60faffdd-b899-4464-9059-6f4f5e54747f",
            "e8ada80c-bee5-4584-a715-aa847c615cd5", "ee56b2f3-fd49-4a29-ae1a-2d321549a633",
            "d91a0e9a-8652-4036-822f-e7b12b381273", "ee683b29-4a5e-4a80-a65c-c71cb6c26d07",
            "a0613b70-293f-4a5b-8169-357be1777c62", "3f8018b9-91cd-4f89-a2e3-cb3783a73dec",
            "3ace01d1-15fc-4fbb-99e4-81a696cb2d05", "012a211b-4c91-451c-8a0e-5e3ab398eda8",
            "e76c93df-31af-49a5-8e2f-752c76c937c9", "de737ffc-306b-4f32-8bbb-34e5d370ec8f",
            "3cba0cb4-e2a6-402c-93ee-c11ffcb127ef", "09199619-0400-4010-af4c-c7d6d6617e70",
            "86ea784f-eed1-4696-ac01-ebb65589bf0e", "a2ab141b-687d-48aa-8638-c6e7951990f9",
            "f8d2730d-1fcb-414e-94bb-0a0f876d7720", "1dd9eb9b-ce6d-451b-b8ef-f39b3435b35a",
            "127eb7ec-9d9f-43bc-a259-9eb03c467fef", "95712b31-ceed-4f7d-be3e-3d5e6badac5c",
            "1d659f1e-9aa9-45ff-8aa9-a75bd55a7c49", "0dd79157-a449-418e-b374-99907c67fe18",
            "63687e52-8d88-4f3c-8817-579ded518a21", "2eb9f5eb-2bff-42d7-8d90-a84bb6a4f16b",
            "09120193-f2db-4073-8ea8-be994e61e5b3", "290942fd-89c9-4680-98df-b86b49752f6a",
            "6cf3560c-7db1-4691-b22b-2bdad0be805f", "5979b17a-4af3-449d-8d9c-3ca312d1f3da",
            "266526e5-70db-46e6-a400-b7d888877e98", "b772df79-1fca-4c0a-a2f3-88b22e63e7c3",
            "81e6f8fc-068c-4d20-9433-2353992cec7a", "704426b6-9fb3-4ff9-8f6b-44d52c551ac3",
            "bd5eefa4-665d-4840-a3f3-eb0232dd7310", "656eac75-9849-43fe-a24d-165e63ffb342",
            "af50079f-9d69-4cbf-955c-6864cab3e39b", "2a55aee6-c87a-4bda-a3d4-b022693a62a3",
            "52ac3730-b955-452a-a7cc-17e3b06182ac", "76eadac1-cdfa-45e5-a25c-54ed4c3a604f",
            "09097de4-344e-4e26-8caa-2518219e1462", "7ad20d41-f0dd-4f37-aad1-9896a69f03f8",
            "a08d9f84-fbdf-45f3-9b68-645d296de307", "d7618a38-f383-4ea6-b77c-9f27912f841b",
            "fae25d37-21e4-4b5f-ae0c-44cf34d6e6d6", "fd329241-ef6c-49e5-b74d-86d9d0ab0f4a",
            "3beaa9a5-6404-4446-bfd1-e0d0eea65e85", "cf4b2ba4-3626-4de7-9d07-abe8a65b1665",
            "40737d3d-0ff6-4a9e-a050-beb87bf90120", "33bd3bad-6803-4c5e-97ac-f7e31261a604",
            "27f9fc86-2341-4bf4-8cbf-67fc16a841f1", "02861a9b-584d-47d5-a8d0-98e05c3b5dce",
            "41aba635-48e4-43bf-9bb1-3b811b5705d0", "352feb94-e52e-45eb-9c92-e4b44c6b1a9d",
            "3a4e0f49-f9ff-4481-a95b-d4952cdf6097", "dd142c0b-622a-4b44-8e22-c0facb2b20a8",
            "5f8057b4-adfd-4258-906d-9ae3485ffbff", "f1c4fa32-1d2a-43c2-910d-053bc3f80bcb",
            "6d74b3d9-f977-4c9b-8114-93969b51b105", "4c31b3ad-5dc9-437f-ac1a-0fdff36a2818",
            "767b8357-ce52-4e09-810f-30ee81f8136c", "9d3adf0b-f0bf-43c6-b92f-90236880d3db",
            "d34121a8-4301-44f2-b887-88b11e5127d8", "aea834d4-4365-4547-bf2e-f05b2adde4c2",
            "5cd0aeed-2e8a-41c8-9a33-55eb5b121962", "d6bb186c-382f-4ae6-bc76-fde8f141ed69",
            "b5724816-0507-4da0-ae22-7c69df8bc871", "5c00cd7c-b207-4d9d-9c8c-a8d418fc5425",
            "0beafb5d-539e-4b4f-a2f4-74f6a442dae0", "05ca3e51-580b-44ca-8046-7d3e57a6d248",
            "d3a99f99-44b1-45b2-9e7c-c3d7317cfcfb", "92524eb7-4ee5-4eab-936c-2eb8e6eb0ecd",
            "bb28947d-a747-44f0-ac71-1fa8188f4cee", "99baf196-9dfc-4b13-af77-025e87ac2a7d",
            "a2d92647-634d-4eb4-886b-9da858e871f1", "7f6e15e9-f2cf-4965-90a5-5aa1f79a24ab",
            "98c7115e-1898-4d4c-8662-8e3a3dfe7c7f")

dat_entity_wdw = wdw_ids %>%
  map(.f = ~tpr_entity(.x) %>% pluck("content"))
