## code to prepare `tpr_datasets_cache` datasets goes here

# destinations
tpr_destinations_data = tpr_destinations()$content
usethis::use_data(tpr_destinations_data, overwrite = TRUE)

# entity
tpr_entity_data = purrr::map_dfr(tpr_destinations()$content$id %>% unique(),
                                 ~tpr_entity(.x) %>% purrr::pluck("content"))
usethis::use_data(tpr_entity_data, overwrite = TRUE)

#children
tpr_entity_children_data = purrr::map_dfr(.x = tpr_destinations()$content$id %>%
                                            unique(),
                                          .f = ~tpr_entity_children(.x) %>%
                                            purrr::pluck("content"))
usethis::use_data(tpr_entity_children_data, overwrite = TRUE)

#live
tpr_entity_live_data = purrr::map_dfr(.x = tpr_destinations()$content$id %>%
                                        unique(),
                                      .f = ~tpr_entity_live(.x) %>%
                                        purrr::pluck("content"))
usethis::use_data(tpr_entity_live_data, overwrite = TRUE)
