#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}
