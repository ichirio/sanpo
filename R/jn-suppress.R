#' Small-cell suppression
#'
#' The suppression hook used by every table (`tbl_pathways()`,
#' `jn_matrix()`, `jn_attrition()`): counts from 1 to `min_cell - 1` are
#' set to `NA`, their label becomes `"<min_cell"`, the matching shares are
#' set to `NA`, and a logical column `suppressed` marks the rows touched.
#' Zero counts are kept (they identify no one and show the empty nodes).
#' Only primary suppression is done; no secondary suppression.
#'
#' The rule can be set once for a project:
#'
#' * option `sanpo.min_cell`: default `min_cell` for every table (for
#'   example `options(sanpo.min_cell = 10)` for a database whose rules ask
#'   for it); `NULL` (default) suppresses nothing.
#' * option `sanpo.suppress`: a function with the arguments of
#'   `jn_suppress()` that replaces the rule, for example to add secondary
#'   suppression.
#'
#' @param x A data frame of counts.
#' @param min_cell `NULL` (use the option `sanpo.min_cell`; no suppression
#'   when it is unset) or a number >= 1.
#' @param cols Count columns to suppress.
#' @param pct Share columns set to `NA` on suppressed rows.
#' @return `x` with the count columns, their `<col>_label` columns (made
#'   when missing) and `suppressed`.
#' @examples
#' d <- data.frame(group = c("a", "b", "c"), n = c(25L, 3L, 0L))
#' jn_suppress(d, min_cell = 10)
#' @export
jn_suppress <- function(x, min_cell = NULL, cols = "n", pct = NULL) {
  min_cell <- min_cell %||% getOption(opt_name("min_cell"))
  if (is.null(min_cell)) return(x)
  if (!is.numeric(min_cell) || length(min_cell) != 1 || is.na(min_cell) || min_cell < 1) {
    cli::cli_abort("{.arg min_cell} must be NULL or a single number >= 1.")
  }
  sup <- rep(FALSE, nrow(x))
  for (cl in cols) {
    lab <- paste0(cl, "_label")
    if (!lab %in% names(x)) x[[lab]] <- format_n(x[[cl]])
    hit <- !is.na(x[[cl]]) & x[[cl]] > 0 & x[[cl]] < min_cell
    x[[cl]][hit] <- NA
    x[[lab]][hit] <- paste0("<", min_cell)
    sup <- sup | hit
  }
  for (p in pct) x[[p]][sup] <- NA
  x$suppressed <- sup
  x
}

# The tables call the hook through here, so that a project-wide rule
# (option `<pkg>.suppress`) replaces jn_suppress().
apply_suppression <- function(x, min_cell, cols, pct = NULL) {
  fun <- getOption(opt_name("suppress"), jn_suppress)
  fun(x, min_cell = min_cell, cols = cols, pct = pct)
}
