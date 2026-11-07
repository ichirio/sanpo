#' Flows between steps
#'
#' Count the patients who move from each state at one step to each state at
#' the next. All pairs of levels are returned, including pairs with `n = 0`,
#' so empty nodes and empty flows are explicit rows.
#'
#' The counts are made with the same `dplyr` code on a sequence in memory
#' and on a sequence in a database (see [engine]); only the aggregated
#' counts are collected.
#'
#' @param j A [journey()].
#' @param steps Steps to use (`NULL` = all). Flows run between successive
#'   elements, so `steps = c(1, 3)` gives the flows from step 1 to step 3.
#' @param engine `"auto"`, `"db"` or `"memory"`; see [engine].
#' @param memory_limit Largest size collected into memory; see [engine].
#' @return A data frame with columns `step_from`, `step_to`, `from`, `to`
#'   (factors with the journey's levels), `n` and `pct_from` (share of the
#'   patients in `from` at `step_from`).
#' @examples
#' j <- jn_sequence(jn_simulate(200), max_steps = 3)
#' fl <- jn_flows(j)
#' fl[fl$n > 0, ]
#' @export
jn_flows <- function(j, steps = NULL, engine = c("auto", "db", "memory"),
                     memory_limit = NULL) {
  check_journey(j)
  s <- journey_data(j, engine, memory_limit, "jn_flows")
  flows_from(s, j@states$level, steps)
}

# Data of a journey's sequence where a verb runs (collected when the verb
# runs in memory), with `state` as character.
journey_data <- function(j, engine, memory_limit, fn) {
  engine <- match.arg(engine, c("auto", "db", "memory"))
  s <- j@sequence
  if (!is_lazy(s) && engine == "db") engine <- "memory"
  ch <- choose_engine(s, engine, memory_limit, fn)
  dplyr::mutate(ch$data, state = as.character(state))
}

# Patients per state at each step (all levels, zeros included).
node_counts <- function(s, lv, steps = NULL) {
  s <- dplyr::mutate(s, state = as.character(state))
  cnt <- as.data.frame(dplyr::collect(dplyr::count(dplyr::ungroup(s), step, state)))
  n_steps <- if (nrow(cnt) == 0) 0L else as.integer(max(cnt$step))
  steps <- resolve_steps(steps, n_steps)
  L <- length(lv)
  out <- data.frame(step = rep(steps, each = L),
                    state = factor(rep(lv, length(steps)), levels = lv))
  n <- cnt$n[match(paste(out$step, out$state), paste(cnt$step, cnt$state))]
  out$n <- as.integer(ifelse(is.na(n), 0, n))
  out
}

flows_from <- function(s, lv, steps = NULL) {
  nodes <- node_counts(s, lv, steps)
  steps <- unique(nodes$step)
  empty <- data.frame(step_from = integer(), step_to = integer(),
                      from = factor(character(), levels = lv),
                      to = factor(character(), levels = lv),
                      n = integer(), pct_from = numeric())
  if (length(steps) < 2) return(empty)
  f <- dplyr::filter(dplyr::ungroup(s), step %in% !!steps)
  f <- win_order(win_part(f), patient_id, step)
  f <- dplyr::mutate(f, next_p = dplyr::lead(patient_id), to = dplyr::lead(state),
                     step_to = dplyr::lead(step))
  f <- dplyr::filter(dplyr::ungroup(f), !is.na(next_p) & next_p == patient_id)
  cnt <- as.data.frame(dplyr::collect(dplyr::count(f, step, step_to, state, to)))
  L <- length(lv)
  k <- length(steps) - 1
  out <- data.frame(step_from = rep(steps[-length(steps)], each = L * L),
                    step_to = rep(steps[-1], each = L * L),
                    from = factor(rep(rep(lv, each = L), k), levels = lv),
                    to = factor(rep(lv, L * k), levels = lv))
  key <- paste(out$step_from, out$from, out$to)
  n <- cnt$n[match(key, paste(cnt$step, cnt$state, cnt$to))]
  out$n <- as.integer(ifelse(is.na(n), 0, n))
  n_from <- nodes$n[match(paste(out$step_from, out$from), paste(nodes$step, nodes$state))]
  out$pct_from <- ifelse(n_from > 0, out$n / n_from, 0)
  out
}

# Patients per pathway over `steps`: one column per step (`s1`, `s2`, ...,
# `NA` after a patient's last step) and `n`. Every patient has a step 1, so
# the step-1 rows give one row per patient.
path_counts <- function(s, steps) {
  s <- dplyr::ungroup(s)
  wide <- dplyr::select(dplyr::filter(s, step == 1L), "patient_id")
  cols <- paste0("s", steps)
  for (i in seq_along(steps)) {
    k <- steps[i]
    one <- dplyr::filter(s, step == !!k)
    one <- dplyr::select(one, "patient_id", !!cols[i] := "state")
    wide <- dplyr::left_join(wide, one, by = "patient_id")
  }
  cnt <- dplyr::collect(dplyr::count(wide, dplyr::across(dplyr::all_of(cols))))
  cnt <- as.data.frame(cnt, stringsAsFactors = FALSE)
  cnt$n <- as.integer(cnt$n)
  cnt
}

# Number of patients: every patient has exactly one step-1 row.
n_persons_of <- function(s) {
  as.integer(count_rows(dplyr::filter(dplyr::ungroup(s), step == 1L)))
}

# Largest step of a sequence (one aggregate query on a lazy table).
max_step <- function(s) {
  m <- dplyr::pull(dplyr::collect(dplyr::summarise(dplyr::ungroup(s), m = max(step, na.rm = TRUE))))
  if (length(m) == 0 || is.na(m) || !is.finite(m)) 0L else as.integer(m)
}

#' Prefix tree of pathways
#'
#' Count the patients sharing each pathway prefix (`A`, `A/B`, `A/B/C`,
#' ...). This is the data behind [plot_sunburst()] and [plot_icicle()]:
#' each node also carries its position as a fraction of all patients
#' (`start`, `end`), with children ordered by the states dictionary inside
#' their parent.
#'
#' The pathway counts are made in memory or in the database with the same
#' `dplyr` code (see [engine]); the tree is built from those counts.
#'
#' @param j A [journey()].
#' @param depth Maximum depth (number of steps); `NULL` = all.
#' @inheritParams jn_flows
#' @return A data frame with columns `id` (the prefix, states joined by
#'   `"/"`), `parent` (`NA` at depth 1), `depth`, `state` (factor), `n`,
#'   `pct` (share of all patients), `start` and `end`.
#' @examples
#' j <- jn_sequence(jn_simulate(200), max_steps = 3)
#' head(jn_tree(j))
#' @export
jn_tree <- function(j, depth = NULL, engine = c("auto", "db", "memory"),
                    memory_limit = NULL) {
  check_journey(j)
  if (!is.null(depth)) {
    depth <- as.integer(max(depth))
    if (is.na(depth) || depth < 1) cli::cli_abort("{.arg depth} must be >= 1.")
  }
  s <- journey_data(j, engine, memory_limit, "jn_tree")
  tree_from(s, j@states$level, depth)
}

tree_from <- function(s, lv, depth = NULL) {
  D <- max_step(s)
  if (!is.null(depth)) D <- min(depth, D)
  p <- path_counts(s, seq_len(D))
  N <- sum(p$n)
  codes_all <- vapply(seq_len(D), function(d) match(p[[d]], lv), integer(nrow(p)))
  codes_all <- matrix(codes_all, nrow = nrow(p))
  out <- vector("list", D)
  for (d in seq_len(D)) {
    ok <- !is.na(codes_all[, d])
    codes <- codes_all[ok, seq_len(d), drop = FALSE]
    key <- apply(matrix(lv[codes], ncol = d), 1, paste, collapse = "/")
    n <- rowsum(p$n[ok], key, reorder = FALSE)
    first <- match(rownames(n), key)
    cd <- codes[first, , drop = FALSE]
    nd <- data.frame(id = rownames(n), n = as.integer(n[, 1]),
                     parent = if (d > 1) {
                       apply(matrix(lv[cd[, seq_len(d - 1)]], ncol = d - 1), 1, paste, collapse = "/")
                     } else NA_character_,
                     depth = d, state = factor(lv[cd[, d]], levels = lv),
                     stringsAsFactors = FALSE)
    nd <- nd[do.call(order, as.data.frame(cd)), ]
    if (d == 1) {
      nd$start <- (cumsum(nd$n) - nd$n) / N
    } else {
      par <- out[[d - 1]]
      off <- stats::ave(nd$n, nd$parent, FUN = function(v) cumsum(v) - v)
      nd$start <- par$start[match(nd$parent, par$id)] + off / N
    }
    nd$end <- nd$start + nd$n / N
    out[[d]] <- nd
  }
  res <- do.call(rbind, out)
  res$pct <- res$n / N
  rownames(res) <- NULL
  res[, c("id", "parent", "depth", "state", "n", "pct", "start", "end")]
}
