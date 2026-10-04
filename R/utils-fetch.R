# Internal: GET a path on the ThemeParks API and parse the JSON body.
# `task` completes the error message, e.g. "Failed to get details."
# Errors are reported against `call`, the exported function the user ran.
tpr_fetch = function(path, task, call = rlang::caller_env()) {
  req = httr2::request("https://api.themeparks.wiki") %>%
    httr2::req_url_path(path) %>%
    httr2::req_user_agent("themeparkr (https://github.com/michaelbgarcia/themeparkr)") %>%
    httr2::req_timeout(30) %>%
    httr2::req_retry(max_tries = 3)

  resp = tryCatch(
    httr2::req_perform(req),
    httr2_http = function(cnd) {
      status = httr2::resp_status(cnd$resp)
      cli::cli_abort(
        c(
          "Failed to {task}.",
          "x" = "{.url {req$url}} returned HTTP {status}.",
          "i" = tpr_error_message(cnd$resp)
        ),
        class = "themeparkr_http_error",
        status = status,
        resp = cnd$resp,
        call = call
      )
    }
  )
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

# Internal: the API's own error message from an error response, or NULL
tpr_error_message = function(resp) {
  tryCatch(httr2::resp_body_json(resp)$error$message, error = function(e) NULL)
}
