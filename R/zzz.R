#' @export
print.themeparks_api <- function(x, ...) {
  cat("<themeparks ", x$path, ">\n", sep = "")
  dplyr::glimpse(x$content)
  invisible(x)
}
