# Internal: GET a path on the ThemeParks API and parse the JSON body.
# `task` completes httr's error message, e.g. "Failed to get details."
tpr_fetch = function(path, task) {
  url = httr::modify_url("https://api.themeparks.wiki", path = path)
  resp = httr::GET(url)
  httr::stop_for_status(resp, task)
  jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
}
