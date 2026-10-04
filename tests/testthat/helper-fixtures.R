# Replace tpr_fetch() so the calling test reads a recorded API response
# from tests/testthat/fixtures/<name>.json instead of hitting the network.
local_fixture = function(name, env = parent.frame()) {
  fixture = test_path("fixtures", paste0(name, ".json"))
  local_mocked_bindings(
    tpr_fetch = function(...) jsonlite::fromJSON(fixture, simplifyVector = FALSE),
    .env = env
  )
}
