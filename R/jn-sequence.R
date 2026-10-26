#' Build a journey from episodes
#'
#' Turn episode rows (one row per prescription, claim or exposure period)
#' into the sequence of states per patient.
#'
#' The rules, applied per patient in order of `start` (then `end`, then
#' `state`, so that ties give the same order in memory and in a database):
#'
#' 1. Rows of the same state whose gap is at most `grace` belong to one
#'    episode. The gap is measured from the latest `end` seen so far; a row
#'    that starts the day (month) after it has gap 0.
#' 2. If `gap_state` is given, a gap longer than `grace` becomes a step of
#'    that state (for example `"(gap)"`, an untreated period).
#' 3. With `collapse = TRUE`, consecutive episodes of the same state become
#'    one step (`A, A, B` -> `A, B`).
#' 4. The sequence is cut at `max_steps`. A patient with fewer steps gets
#'    one more step, `end_state`, so that the end of follow-up is a node of
#'    its own rather than a missing value.
#'
#' Every parameter is stored in `meta` and printed with the journey.
#'
#' `data` may be a data frame or a lazy database table (`dbplyr`); the same
#' `dplyr` code runs on both and gives the same journey. See [engine] for
#' `engine` and `memory_limit`: with `"auto"`, a lazy table under the limit
#' is collected first; over it the sequence is built in the database and
#' collected when it fits under the limit (otherwise the journey keeps a
#' lazy sequence, materialised as a temporary table when the database
#' allows it).
#'
#' @param data A data frame or a lazy table of episodes.
#' @param id,start,end,state Column names (strings) in `data`. `end` may be
#'   `NA` (a single day or month). `start` and `end` are `Date`s.
#' @param unit Time unit of the data: `"day"` or `"month"` (claims that only
#'   carry the month; dates are floored to the first of the month).
#' @param grace Largest gap, in `unit`s, that still counts as continuous.
#'   Defaults to 30 days or 1 month.
#' @param gap_state `NULL` (default), or the name of a state inserted for
#'   gaps longer than `grace`.
#' @param collapse Collapse consecutive equal states into one step.
#' @param max_steps Maximum number of steps per patient (`Inf` for all).
#' @param end_state Name of the state appended after a patient's last step,
#'   or `NULL` for none.
#' @param states A [jn_states()] dictionary or a character vector of levels
#'   (the node order). `NULL` uses the levels of `data[[state]]` when it is
#'   a factor, otherwise its sorted unique values (a database has no
#'   factors, so pass `states` to keep the order and the empty nodes).
#'   Levels no row uses are kept as empty nodes. `gap_state` and
#'   `end_state` are appended when missing.
#' @param engine `"auto"`, `"db"` or `"memory"`; see [engine].
#' @param memory_limit Largest size collected into memory; defaults to the
#'   option `sanpo.memory_limit` (`"2GB"`). See [engine].
#' @return A [journey()].
#' @examples
#' ep <- jn_simulate(200)
#' j <- jn_sequence(ep, max_steps = 3)
#' j
#' @export
jn_sequence <- function(data, id = "patient_id", start = "start", end = "end",
                        state = "state", unit = c("day", "month"),
                        grace = NULL, gap_state = NULL, collapse = TRUE,
                        max_steps = 4, end_state = "(end)", states = NULL,
                        engine = c("auto", "db", "memory"), memory_limit = NULL) {
  unit <- match.arg(unit)
  engine <- match.arg(engine)
  if (!is.data.frame(data) && !is_lazy(data)) {
    cli::cli_abort("{.arg data} must be a data frame or a lazy database table.")
  }
  proto <- col_proto(data)
  cols <- c(id, start, end, state)
  miss <- setdiff(cols, names(proto))
  if (length(miss) > 0) cli::cli_abort("Column{?s} {.val {miss}} not found in {.arg data}.")
  if (is.null(grace)) grace <- if (unit == "day") 30 else 1
  if (!is.numeric(grace) || length(grace) != 1 || is.na(grace) || grace < 0) {
    cli::cli_abort("{.arg grace} must be a single number >= 0.")
  }
  if (!is.numeric(max_steps) || length(max_steps) != 1 || is.na(max_steps) || max_steps < 1) {
    cli::cli_abort("{.arg max_steps} must be a single number >= 1.")
  }
  if (!inherits(proto[[start]], "Date")) cli::cli_abort("Column {.field {start}} must be a {.cls Date}.")
  if (!inherits(proto[[end]], "Date")) {
    if (!is_lazy(data) && all(is.na(data[[end]]))) {
      data[[end]] <- as.Date(rep(NA_real_, nrow(data)))
    } else {
      cli::cli_abort("Column {.field {end}} must be a {.cls Date}.")
    }
  }

  # states dictionary -------------------------------------------------------
  if (is.null(states)) {
    lv <- if (is.factor(proto[[state]])) levels(proto[[state]]) else NULL
  }
  ch <- choose_engine(data, engine, memory_limit, "jn_sequence")
  x <- dplyr::select(ch$data, patient_id = dplyr::all_of(id),
                     state = dplyr::all_of(state), start = dplyr::all_of(start),
                     end = dplyr::all_of(end))
  x <- dplyr::mutate(x, state = as.character(state))
  if (is.null(states)) {
    if (is.null(lv)) lv <- sort(distinct_values(x, "state"))
    states <- jn_states(lv)
  } else {
    states <- as_jn_states(states)
  }
  extra <- setdiff(c(gap_state, end_state), states$level)
  if (length(extra) > 0) {
    states <- jn_states(c(states$level, extra), c(states$label, extra),
                        c(states$colour, jn_default_colours(c(states$level, extra))[-seq_len(nrow(states))]))
  }
  n_miss <- count_where(x, is.na(patient_id) | is.na(state) | is.na(start))
  if (n_miss > 0) {
    cli::cli_abort("{.field {id}}, {.field {state}} and {.field {start}} must not be missing.")
  }
  bad <- setdiff(distinct_values(x, "state"), states$level)
  if (length(bad) > 0) {
    cli::cli_abort("State{?s} {.val {bad}} {?is/are} not in {.arg states}.")
  }

  seq <- sequence_steps(x, unit = unit, grace = grace, gap_state = gap_state,
                        collapse = collapse, max_steps = max_steps,
                        end_state = end_state, end_col = end)
  meta <- list(unit = unit, grace = grace, gap_state = gap_state,
               collapse = collapse, max_steps = max_steps,
               end_state = end_state)
  finish_sequence(seq, ch$engine, engine, memory_limit, states, meta, "jn_sequence")
}

# A sequence built where `used` says -> journey. Built in the database with
# engine "auto": materialise it, then collect it when it fits under the
# limit, so that later verbs run in memory.
finish_sequence <- function(seq, used, engine, memory_limit, states, meta, fn) {
  if (used == "db") {
    seq <- try_compute(seq)
    if (engine == "auto") {
      sz <- jn_size(seq, memory_limit)
      if (sz$bytes <= sz$limit) {
        engine_message(fn, "memory", sz, "sequence built in the database and collected")
        seq <- dplyr::collect(seq)
      }
    }
  }
  seq_to_journey(seq, states, meta)
}

# The shared dplyr pipeline: episodes (patient_id, state, start, end) ->
# sequence (patient_id, step, state, start, end). Runs unchanged on a data
# frame and on a lazy table.
sequence_steps <- function(x, unit, grace, gap_state, collapse, max_steps,
                           end_state, end_col = "end") {
  x <- add_time_index(x, unit, end_col)
  steps_from_index(x, unit, grace, gap_state, collapse, max_steps, end_state)
}

# start / end (Date) -> t0 / t1: integer days since 1970-01-01, or months
# since year 0; a missing end is the start.
add_time_index <- function(x, unit, end_col = "end") {
  x <- if (unit == "day") {
    dplyr::mutate(x, t0 = as.integer(start - as.Date("1970-01-01")),
                  t1 = as.integer(end - as.Date("1970-01-01")))
  } else {
    dplyr::mutate(x, t0 = as.integer(year(start) * 12L + month(start) - 1L),
                  t1 = as.integer(year(end) * 12L + month(end) - 1L))
  }
  x <- dplyr::mutate(x, t1 = dplyr::coalesce(t1, t0))
  if (count_where(x, t1 < t0) > 0) {
    cli::cli_abort("{.field {end_col}} is before {.field start} in some rows.", call = NULL)
  }
  dplyr::select(x, -"start", -"end")
}

# t0 / t1 -> Date.
index_to_date <- function(x, unit) {
  if (unit == "day") {
    dplyr::mutate(x, start = as.Date("1970-01-01") + t0,
                  end = as.Date("1970-01-01") + t1)
  } else {
    dplyr::mutate(x,
      start = make_date(as.integer(floor(t0 / 12)), as.integer(t0 - floor(t0 / 12) * 12) + 1L, 1L),
      end = make_date(as.integer(floor(t1 / 12)), as.integer(t1 - floor(t1 / 12) * 12) + 1L, 1L)
    )
  }
}

# Rows (patient_id, state, t0, t1) -> steps (patient_id, step, state, start,
# end).
steps_from_index <- function(x, unit, grace, gap_state, collapse, max_steps,
                             end_state) {
  x <- dplyr::select(x, "patient_id", "state", "t0", "t1")

  # 1. episodes: same state and gap <= grace --------------------------------
  # Windows run over rows ordered by patient, partitioned per patient in a
  # database and not at all in memory (see win_part()): `p` numbers the
  # patients, blocks number the episodes, and block_cummax() gives running
  # maxima within them. The result does not depend on the partition.
  x <- win_order(win_part(x), patient_id, t0, t1, state)
  x <- mark_blocks(x, "patient_id", "new_p", "p")
  x <- block_cummax(x, "run_end", "t1", "p")
  x <- dplyr::mutate(x, prev_end = dplyr::lag(run_end), prev_state = dplyr::lag(state))
  x <- dplyr::mutate(x, new_run = dplyr::if_else(
    new_p == 0L & state == prev_state & t0 - prev_end - 1L <= !!grace, 0L, 1L
  ))
  runs <- collapse_blocks(x, "new_run")
  runs <- checkpoint(dplyr::mutate(runs, ord = as.integer(block * 2L)))

  # 2. gaps between episodes ------------------------------------------------
  if (!is.null(gap_state)) {
    runs <- win_order(win_part(runs), patient_id, ord)
    runs <- mark_blocks(runs, "patient_id", "new_p", "p")
    runs <- block_cummax(runs, "cm", "t1", "p")
    runs <- dplyr::mutate(runs, prev = dplyr::lag(cm))
    gaps <- dplyr::filter(runs, new_p == 0L & t0 - prev - 1L > !!grace)
    gaps <- dplyr::mutate(gaps, state = !!gap_state, g0 = as.integer(prev + 1L),
                          g1 = as.integer(t0 - 1L), ord = as.integer(ord - 1L))
    gaps <- dplyr::select(gaps, patient_id, state, t0 = g0, t1 = g1, ord)
    runs <- checkpoint(dplyr::union_all(dplyr::select(dplyr::ungroup(runs), patient_id, state, t0, t1, ord),
                                        dplyr::ungroup(gaps)))
  } else {
    runs <- dplyr::select(runs, patient_id, state, t0, t1, ord)
  }

  # 3. collapse consecutive equal states ------------------------------------
  if (collapse) {
    runs <- win_order(win_part(runs), patient_id, ord)
    runs <- mark_blocks(runs, "patient_id", "new_p", "p")
    runs <- dplyr::mutate(runs, prev_state = dplyr::lag(state))
    runs <- dplyr::mutate(runs, new = dplyr::if_else(new_p == 0L & state == prev_state, 0L, 1L))
    runs <- collapse_blocks(runs, "new", first = c(t0 = "t0", ord = "ord"))
  }

  # 4. steps, max_steps, end state ------------------------------------------
  s <- win_order(win_part(dplyr::ungroup(runs)), patient_id, ord)
  s <- mark_blocks(s, "patient_id", "new_p", "p")
  s <- dplyr::mutate(s, rn = dplyr::row_number())
  s <- dplyr::mutate(s, first_rn = cummax(rn * new_p))
  s <- dplyr::mutate(s, step = as.integer(rn - first_rn + 1L))
  if (is.finite(max_steps)) s <- dplyr::filter(s, step <= !!max_steps)
  if (!is.null(end_state)) {
    s <- checkpoint(dplyr::select(dplyr::ungroup(s), patient_id, step, state, t0, t1))
    s <- win_order(win_part(s), patient_id, step)
    s <- dplyr::mutate(s, next_p = dplyr::lead(patient_id))
    s <- dplyr::ungroup(s)
    ends <- dplyr::filter(s, is.na(next_p) | next_p != patient_id)
    if (is.finite(max_steps)) ends <- dplyr::filter(ends, step < !!max_steps)
    ends <- dplyr::mutate(ends, state = !!end_state, step = as.integer(step + 1L),
                          t0 = t1, t1 = NA_integer_)
    s <- dplyr::union_all(dplyr::select(s, patient_id, step, state, t0, t1),
                          dplyr::select(ends, patient_id, step, state, t0, t1))
  }

  s <- index_to_date(dplyr::select(dplyr::ungroup(s), patient_id, step, state, t0, t1), unit)
  dplyr::select(s, "patient_id", "step", "state", "start", "end")
}

# Window helpers on rows in a fixed window order (see win_part()) -----------

# `flag` = 1 on the first row of each block of equal `col` values (one or
# more columns), `id` = block number (1, 2, ...).
mark_blocks <- function(x, col, flag, id) {
  prev <- paste0(".prev_", seq_along(col))
  for (i in seq_along(col)) {
    x <- dplyr::mutate(x, !!prev[i] := dplyr::lag(!!rlang::sym(col[i])))
  }
  same <- Reduce(function(a, b) rlang::expr(!!a & !!b), Map(function(p, cl) {
    rlang::expr(!is.na(!!rlang::sym(p)) & !!rlang::sym(p) == !!rlang::sym(cl))
  }, prev, col))
  x <- dplyr::mutate(x, !!flag := dplyr::if_else(!!same, 0L, 1L))
  x <- dplyr::mutate(x, !!id := cumsum(!!rlang::sym(flag)))
  dplyr::select(x, -dplyr::all_of(prev))
}

# Running maximum of `col` within blocks numbered by `block` (non-decreasing
# in the window order): cummax(block * K + col) - block * K. K = 2^22 is
# larger than twice any day or month index, and block * K stays exact in
# doubles up to 2^31 blocks.
block_k <- 4194304

block_cummax <- function(x, name, col, block) {
  col <- rlang::sym(col)
  block <- rlang::sym(block)
  dplyr::mutate(x, !!name := as.integer(cummax(!!block * !!block_k + !!col) - !!block * !!block_k))
}

# Rows -> one row per block (a block starts where `flag` is 1), kept at the
# block's last row: `keep` (constant within a block, such as patient_id and
# state), `block` = the block number (per partition), the values of `first`
# from the block's first row and the largest values of `max` (both named
# vectors: output name = input column; integer columns). Same result as
# group_by() + summarise(), with windows only (no self-join, which would
# repeat the whole query in SQL).
collapse_blocks <- function(x, flag, first = c(t0 = "t0"), max = c(t1 = "t1"),
                            keep = c("patient_id", "state")) {
  f <- rlang::sym(flag)
  x <- dplyr::mutate(x, block = cumsum(!!f))
  # new columns only: the window order may use the input columns
  tmp_max <- paste0(".max_", names(max))
  for (i in seq_along(max)) x <- block_cummax(x, tmp_max[i], max[[i]], "block")
  tmp_first <- paste0(".first_", names(first))
  for (i in seq_along(first)) {
    v <- rlang::sym(first[[i]])
    # at first rows block * K + value (> 0 as K > |value|), else 0
    x <- dplyr::mutate(x, .first = dplyr::if_else(!!f == 1L, !!v + block * !!block_k, 0))
    x <- dplyr::mutate(x, !!tmp_first[i] := as.integer(cummax(.first) - block * !!block_k))
  }
  x <- dplyr::mutate(x, .next = dplyr::lead(block))
  x <- dplyr::filter(dplyr::ungroup(x), is.na(.next) | .next != block)
  x <- dplyr::select(x, dplyr::all_of(keep), "block", dplyr::all_of(c(tmp_first, tmp_max)))
  dplyr::rename(x, !!!rlang::set_names(rlang::syms(c(tmp_first, tmp_max)),
                                       c(names(first), names(max))))
}

# Materialise a lazy query (a temporary table) where it is reused, when the
# database allows it; a data frame is returned as it is.
checkpoint <- function(x) {
  if (is_lazy(x)) try_compute(x) else x
}

# Partition for window functions: per patient on a lazy table (the
# database computes patients in parallel), none in memory (one vectorised
# pass; grouped windows in dplyr run once per patient and are slow). The
# window code gives the same result either way.
win_part <- function(x) {
  if (is_lazy(x)) dplyr::group_by(x, patient_id) else dplyr::ungroup(x)
}

# Number of rows where `cond` is TRUE (one aggregate query on a lazy table).
count_where <- function(x, cond) {
  cond <- rlang::enquo(cond)
  r <- dplyr::summarise(dplyr::ungroup(x), n = sum(dplyr::if_else(!!cond, 1L, 0L), na.rm = TRUE))
  n <- dplyr::pull(dplyr::collect(r))
  if (length(n) == 0 || is.na(n)) 0 else as.numeric(n)
}

distinct_values <- function(x, col) {
  v <- dplyr::pull(dplyr::collect(dplyr::distinct(dplyr::ungroup(x), dplyr::across(dplyr::all_of(col)))))
  as.character(v[!is.na(v)])
}

# Materialise a lazy query as a temporary table when the database allows
# it, so that later verbs do not re-run the whole pipeline.
try_compute <- function(x) {
  tryCatch(dplyr::compute(x), error = function(e) x)
}
