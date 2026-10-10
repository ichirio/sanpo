#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data :=
## usethis namespace: end
NULL

# `a`, unless it is NULL, empty or a single NA; then `b`.
`%or%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
