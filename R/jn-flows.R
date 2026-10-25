#' Flows between steps
#'
#' Count the patients who move from each state at one step to each state at
#' the next. All pairs of levels are returned, including pairs with `n = 0`,
#' so empty nodes and empty flows are explicit rows.
#'
#' @param j A [journey()].
#' @param steps Steps to use (`NULL` = all). Flows run between successive
#'   elements, so `steps = c(1, 3)` gives the flows from step 1 to step 3.
#' @return A data frame with columns `step_from`, `step_to`, `from`, `to`
#'   (factors with the journey's levels), `n` and `pct_from` (share of the
#'   patients in `from` at `step_from`).
#' @examples
#' j <- jn_sequence(jn_simulate(200), max_steps = 3)
#' fl <- jn_flows(j)
#' fl[fl$n > 0, ]
#' @export
jn_flows <- function(j, steps = NULL) {
  check_journey(j)
  m <- jn_state_matrix(j)
  steps <- resolve_steps(steps, ncol(m))
  lv <- j$states$level
  L <- length(lv)
  if (length(steps) < 2) {
    return(data.frame(step_from = integer(), step_to = integer(),
                      from = factor(character(), levels = lv),
                      to = factor(character(), levels = lv),
                      n = integer(), pct_from = numeric()))
  }
  out <- lapply(seq_len(length(steps) - 1), function(i) {
    a <- m[, steps[i]]
    b <- m[, steps[i + 1]]
    ok <- !is.na(a) & !is.na(b)
    n <- tabulate((a[ok] - 1L) * L + b[ok], nbins = L * L)
    n_from <- tabulate(a[!is.na(a)], nbins = L)
    from <- rep(seq_len(L), each = L)
    data.frame(step_from = steps[i], step_to = steps[i + 1],
               from = factor(lv[from], levels = lv),
               to = factor(lv[rep(seq_len(L), L)], levels = lv),
               n = n,
               pct_from = ifelse(n_from[from] > 0, n / n_from[from], 0))
  })
  do.call(rbind, out)
}

# Patients per state at each step (all levels, zeros included).
jn_nodes <- function(j, steps = NULL) {
  m <- jn_state_matrix(j)
  steps <- resolve_steps(steps, ncol(m))
  lv <- j$states$level
  L <- length(lv)
  out <- lapply(steps, function(s) {
    data.frame(step = s, state = factor(lv, levels = lv),
               n = tabulate(m[, s][!is.na(m[, s])], nbins = L))
  })
  do.call(rbind, out)
}

#' Prefix tree of pathways
#'
#' Count the patients sharing each pathway prefix (`A`, `A/B`, `A/B/C`,
#' ...). This is the data behind [plot_sunburst()] and [plot_icicle()]:
#' each node also carries its position as a fraction of all patients
#' (`start`, `end`), with children ordered by the states dictionary inside
#' their parent.
#'
#' @param j A [journey()].
#' @param depth Maximum depth (number of steps); `NULL` = all.
#' @return A data frame with columns `id` (the prefix, states joined by
#'   `"/"`), `parent` (`NA` at depth 1), `depth`, `state` (factor), `n`,
#'   `pct` (share of all patients), `start` and `end`.
#' @examples
#' j <- jn_sequence(jn_simulate(200), max_steps = 3)
#' head(jn_tree(j))
#' @export
jn_tree <- function(j, depth = NULL) {
  check_journey(j)
  m <- jn_state_matrix(j)
  if (!is.null(depth)) {
    depth <- as.integer(max(depth))
    if (is.na(depth) || depth < 1) cli::cli_abort("{.arg depth} must be >= 1.")
    m <- m[, seq_len(min(depth, ncol(m))), drop = FALSE]
  }
  lv <- j$states$level
  N <- nrow(m)
  out <- vector("list", ncol(m))
  key_prev <- rep("", N)
  for (d in seq_len(ncol(m))) {
    ok <- !is.na(m[, d])
    key <- ifelse(ok, paste0(key_prev, if (d > 1) "/" else "", lv[m[, d]]), NA)
    # first patient of each prefix gives the code path used for ordering
    k_ok <- key[ok]
    tab <- table(k_ok)
    first <- match(names(tab), k_ok)
    rows_ok <- which(ok)[first]
    codes <- m[rows_ok, seq_len(d), drop = FALSE]
    nd <- data.frame(id = names(tab), n = as.integer(tab),
                     parent = if (d > 1) key_prev[rows_ok] else NA_character_,
                     depth = d, state = factor(lv[codes[, d]], levels = lv),
                     stringsAsFactors = FALSE)
    o <- do.call(order, as.data.frame(codes))
    nd <- nd[o, ]
    if (d == 1) {
      nd$start <- (cumsum(nd$n) - nd$n) / N
    } else {
      par <- out[[d - 1]]
      off <- stats::ave(nd$n, nd$parent, FUN = function(v) cumsum(v) - v)
      nd$start <- par$start[match(nd$parent, par$id)] + off / N
    }
    nd$end <- nd$start + nd$n / N
    out[[d]] <- nd
    key_prev <- ifelse(ok, key, key_prev)
  }
  res <- do.call(rbind, out)
  res$pct <- res$n / N
  rownames(res) <- NULL
  res[, c("id", "parent", "depth", "state", "n", "pct", "start", "end")]
}
