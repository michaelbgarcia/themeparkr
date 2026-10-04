test_that("tpr_fetch() parses a JSON response into a list", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(id = "abc", children = list()))
  })

  out = tpr_fetch("v1/entity/abc/children", "get list of children")
  expect_equal(out, list(id = "abc", children = list()))
})

test_that("tpr_fetch() identifies the package in its user agent", {
  seen = NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    httr2::response_json(body = list())
  })

  tpr_fetch("v1/destinations", "get destinations")
  expect_match(seen$options$useragent, "^themeparkr")
  expect_equal(seen$url, "https://api.themeparks.wiki/v1/destinations")
})

test_that("HTTP errors name the calling function and include the API message", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(
      status_code = 404,
      body = list(success = FALSE, error = list(message = "Entity notreal not found", code = 404))
    )
  })

  expect_error(tpr_entity("notreal"), class = "themeparkr_http_error")
  expect_snapshot(tpr_entity("notreal"), error = TRUE)
})
