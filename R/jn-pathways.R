#' Pathway counts table
#'
#' Count the patients per pathway (the states of their steps joined in
#' order), largest first. The counts are made in memory or in the database
#' with the same `dplyr` code (see [engine]).
#'
#' @param j A [journey()].
#' @param steps Steps to use (`NULL` = all).
#' @param include_end Keep the end state at the end of each pathway.
#' @param sep Separator between states.
#' @param min_cell `NULL` (default: the option `sanpo.min_cell`, no
#'   suppression when unset) or a number: counts below it are suppressed
#'   (`n` and `pct` set to `NA`, `n_label` shows `"<min_cell"`) by the
#'   suppression hook [jn_suppress()]. Rows are suppressed one by one; no
#'   secondary suppression is done.
#' @inheritParams jn_flows
#' @return A data frame with columns `pathway`, `n_steps`, `n`, `pct`
#'   (share of all patients) and `n_label`, plus `suppressed` when
#'   `min_cell` is given.
#' @examples
#' j <- jn_sequence(jn_simulate(300), max_steps = 3)
#' head(tbl_pathways(j))
#' head(tbl_pathways(j, min_cell = 10))
#' @export
tbl_pathways <- function(j, steps = NULL, include_end = FALSE, sep = " -> ",
                         min_cell = NULL, engine = c("auto", "db", "memory"),
                         memory_limit = NULL) {
  check_journey(j)
  s <- journey_data(j, engine, memory_limit, "tbl_pathways")
  steps <- resolve_steps(steps, max_step(s))
  p <- path_counts(s, steps)
  N <- sum(p$n)
  lab <- j@states$label
  lv <- j@states$level
  m <- matrix(vapply(seq_along(steps), function(k) match(p[[k]], lv), integer(nrow(p))),
              nrow = nrow(p))
  if (!include_end && !is.null(j@meta$end_state)) {
    m[m == match(j@meta$end_state, lv)] <- NA
  }
  lbl <- matrix(lab[m], nrow = nrow(p))
  n_steps <- rowSums(!is.na(m))
  path <- rep(NA_character_, nrow(p))
  for (k in seq_len(ncol(lbl))) {
    l <- lbl[, k]
    path <- ifelse(is.na(l), path, ifelse(is.na(path), l, paste0(path, sep, l)))
  }
  path[is.na(path)] <- "(none)"
  n <- rowsum(p$n, path, reorder = FALSE)
  out <- data.frame(pathway = rownames(n), n = as.integer(n[, 1]), stringsAsFactors = FALSE)
  out$n_steps <- n_steps[match(out$pathway, path)]
  out$pct <- out$n / N
  out <- out[order(-out$n, out$pathway), c("pathway", "n_steps", "n", "pct")]
  out$n_label <- format(out$n, big.mark = ",", trim = TRUE)
  out <- apply_suppression(out, min_cell, cols = "n", pct = "pct")
  rownames(out) <- NULL
  attr(out, "n_persons") <- N
  out
}
