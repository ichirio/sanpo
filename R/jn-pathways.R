#' Pathway counts table
#'
#' Count the patients per pathway (the states of their steps joined in
#' order), largest first.
#'
#' @param j A [journey()].
#' @param steps Steps to use (`NULL` = all).
#' @param include_end Keep the end state at the end of each pathway.
#' @param sep Separator between states.
#' @param min_cell `NULL` (default, no suppression) or a number: counts
#'   below it are suppressed (`n` and `pct` set to `NA`, `n_label` shows
#'   `"<min_cell"`). Rows are suppressed one by one; no secondary
#'   suppression is done.
#' @return A data frame with columns `pathway`, `n_steps`, `n`, `pct`
#'   (share of all patients) and `n_label`, plus `suppressed` when
#'   `min_cell` is given.
#' @examples
#' j <- jn_sequence(jn_simulate(300), max_steps = 3)
#' head(tbl_pathways(j))
#' head(tbl_pathways(j, min_cell = 10))
#' @export
tbl_pathways <- function(j, steps = NULL, include_end = FALSE, sep = " -> ",
                         min_cell = NULL) {
  check_journey(j)
  m <- jn_state_matrix(j)
  steps <- resolve_steps(steps, ncol(m))
  m <- m[, steps, drop = FALSE]
  lab <- j$states$label
  if (!include_end && !is.null(j$meta$end_state)) {
    m[m == match(j$meta$end_state, j$states$level)] <- NA
  }
  N <- nrow(m)
  lbl <- matrix(lab[m], nrow = N)
  n_steps <- rowSums(!is.na(m))
  path <- rep(NA_character_, N)
  for (k in seq_len(ncol(lbl))) {
    l <- lbl[, k]
    path <- ifelse(is.na(l), path, ifelse(is.na(path), l, paste0(path, sep, l)))
  }
  path[is.na(path)] <- "(none)"
  tab <- table(path)
  out <- data.frame(pathway = names(tab), n = as.integer(tab), stringsAsFactors = FALSE)
  out$n_steps <- n_steps[match(out$pathway, path)]
  out$pct <- out$n / N
  out <- out[order(-out$n, out$pathway), c("pathway", "n_steps", "n", "pct")]
  out$n_label <- format(out$n, big.mark = ",", trim = TRUE)
  if (!is.null(min_cell)) {
    if (!is.numeric(min_cell) || length(min_cell) != 1 || is.na(min_cell) || min_cell < 1) {
      cli::cli_abort("{.arg min_cell} must be NULL or a single number >= 1.")
    }
    out$suppressed <- out$n < min_cell
    out$n[out$suppressed] <- NA_integer_
    out$pct[out$suppressed] <- NA_real_
    out$n_label[out$suppressed] <- paste0("<", min_cell)
  }
  rownames(out) <- NULL
  attr(out, "n_persons") <- N
  out
}
