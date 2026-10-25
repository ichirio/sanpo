#' Build a journey from episodes
#'
#' Turn episode rows (one row per prescription, claim or exposure period)
#' into the sequence of states per patient.
#'
#' The rules, applied per patient in order of `start`:
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
#' @param data A data frame of episodes.
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
#'   a factor, otherwise its sorted unique values. Levels no row uses are
#'   kept as empty nodes. `gap_state` and `end_state` are appended when
#'   missing.
#' @return A [journey()].
#' @examples
#' ep <- jn_simulate(200)
#' j <- jn_sequence(ep, max_steps = 3)
#' j
#' @export
jn_sequence <- function(data, id = "patient_id", start = "start", end = "end",
                        state = "state", unit = c("day", "month"),
                        grace = NULL, gap_state = NULL, collapse = TRUE,
                        max_steps = 4, end_state = "(end)", states = NULL) {
  unit <- match.arg(unit)
  if (!is.data.frame(data)) cli::cli_abort("{.arg data} must be a data frame.")
  cols <- c(id, start, end, state)
  miss <- setdiff(cols, names(data))
  if (length(miss) > 0) cli::cli_abort("Column{?s} {.val {miss}} not found in {.arg data}.")
  if (is.null(grace)) grace <- if (unit == "day") 30 else 1
  if (!is.numeric(grace) || length(grace) != 1 || is.na(grace) || grace < 0) {
    cli::cli_abort("{.arg grace} must be a single number >= 0.")
  }
  if (!is.numeric(max_steps) || length(max_steps) != 1 || is.na(max_steps) || max_steps < 1) {
    cli::cli_abort("{.arg max_steps} must be a single number >= 1.")
  }

  pid <- data[[id]]
  st <- data[[state]]
  t0 <- data[[start]]
  t1 <- data[[end]]
  if (!inherits(t0, "Date")) cli::cli_abort("Column {.field {start}} must be a {.cls Date}.")
  if (!inherits(t1, "Date") && !all(is.na(t1))) cli::cli_abort("Column {.field {end}} must be a {.cls Date}.")
  if (anyNA(pid) || anyNA(st) || anyNA(t0)) {
    cli::cli_abort("{.field {id}}, {.field {state}} and {.field {start}} must not be missing.")
  }
  t1 <- as.Date(ifelse(is.na(t1), t0, t1), origin = "1970-01-01")

  # states dictionary -------------------------------------------------------
  if (is.null(states)) {
    lv <- if (is.factor(st)) levels(st) else sort(unique(as.character(st)))
    states <- jn_states(lv)
  } else {
    states <- as_jn_states(states)
  }
  extra <- setdiff(c(gap_state, end_state), states$level)
  if (length(extra) > 0) {
    states <- jn_states(c(states$level, extra), c(states$label, extra),
                        c(states$colour, jn_default_colours(c(states$level, extra))[-seq_len(nrow(states))]))
  }
  code <- match(as.character(st), states$level)
  if (anyNA(code)) {
    bad <- unique(as.character(st)[is.na(code)])
    cli::cli_abort("State{?s} {.val {bad}} {?is/are} not in {.arg states}.")
  }

  # time index in units -----------------------------------------------------
  ti0 <- time_index(t0, unit)
  ti1 <- time_index(t1, unit)
  if (any(ti1 < ti0)) cli::cli_abort("{.field {end}} is before {.field {start}} in some rows.")

  ord <- order(pid, ti0, ti1)
  pid <- pid[ord]
  code <- code[ord]
  ti0 <- ti0[ord]
  ti1 <- ti1[ord]
  n <- length(pid)
  new_p <- c(TRUE, pid[-1] != pid[-n])
  p_idx <- cumsum(new_p)

  # 1. episodes: same state and gap <= grace --------------------------------
  run_end <- group_cummax(ti1, p_idx)
  prev_end <- c(NA, run_end[-n])
  gap <- ti0 - prev_end - 1
  new_run <- new_p | c(TRUE, code[-1] != code[-n]) | gap > grace
  r_idx <- cumsum(new_run)
  r_first <- which(new_run)
  r_last <- c(r_first[-1] - 1L, n)
  r_end <- group_cummax(ti1, r_idx)[r_last]
  runs <- data.frame(p = p_idx[r_first], code = code[r_first],
                     t0 = ti0[r_first], t1 = r_end)

  # 2. gaps between episodes ------------------------------------------------
  nr <- nrow(runs)
  if (!is.null(gap_state) && nr > 1) {
    rp_new <- c(TRUE, runs$p[-1] != runs$p[-nr])
    prev <- c(NA, group_cummax(runs$t1, runs$p)[-nr])
    g <- runs$t0 - prev - 1
    ins <- which(!rp_new & g > grace)
    if (length(ins) > 0) {
      gaps <- data.frame(p = runs$p[ins], code = match(gap_state, states$level),
                         t0 = prev[ins] + 1, t1 = runs$t0[ins] - 1)
      key <- c(seq_len(nr), ins - 0.5)
      runs <- rbind(runs, gaps)[order(key), ]
    }
  }

  # 3. collapse consecutive equal states ------------------------------------
  nr <- nrow(runs)
  if (collapse && nr > 1) {
    same <- c(FALSE, runs$p[-1] == runs$p[-nr] & runs$code[-1] == runs$code[-nr])
    k <- cumsum(!same)
    keep <- which(!same)
    last <- c(keep[-1] - 1L, nr)
    t1c <- group_cummax(runs$t1, k)[last]
    runs <- runs[keep, ]
    runs$t1 <- t1c
  }

  # 4. steps, max_steps, end state ------------------------------------------
  nr <- nrow(runs)
  first <- c(TRUE, runs$p[-1] != runs$p[-nr])
  runs$step <- seq_len(nr) - cummax(ifelse(first, seq_len(nr), 0L)) + 1L
  runs <- runs[runs$step <= max_steps, ]
  if (!is.null(end_state)) {
    nr <- nrow(runs)
    last <- c(runs$p[-1] != runs$p[-nr], TRUE)
    add <- runs[last & runs$step < max_steps, ]
    if (nrow(add) > 0) {
      add$code <- match(end_state, states$level)
      add$step <- add$step + 1L
      add$t0 <- add$t1
      add$t1 <- NA
      runs <- rbind(runs, add)
      runs <- runs[order(runs$p, runs$step), ]
    }
  }

  ids <- unique(pid)
  sequence <- data.frame(
    patient_id = ids[runs$p],
    step = runs$step,
    state = factor(states$level[runs$code], levels = states$level),
    start = index_date(runs$t0, unit),
    end = index_date(runs$t1, unit),
    stringsAsFactors = FALSE
  )
  rownames(sequence) <- NULL
  meta <- list(unit = unit, grace = grace, gap_state = gap_state,
               collapse = collapse, max_steps = max_steps,
               end_state = end_state)
  journey(sequence, states, meta)
}

# Integer time index: days since 1970-01-01, or months since year 0.
time_index <- function(x, unit) {
  if (unit == "day") return(as.numeric(x))
  lt <- as.POSIXlt(x)
  (lt$year + 1900) * 12 + lt$mon
}

index_date <- function(i, unit) {
  if (unit == "day") return(as.Date(i, origin = "1970-01-01"))
  out <- rep(as.Date(NA), length(i))
  ok <- !is.na(i)
  out[ok] <- as.Date(sprintf("%04d-%02d-01", i[ok] %/% 12, i[ok] %% 12 + 1))
  out
}

# Running maximum of x within consecutive groups g (g sorted, integer).
group_cummax <- function(x, g) {
  if (length(x) == 0) return(x)
  lo <- min(x)
  span <- max(x) - lo + 1
  y <- (x - lo) + (g - 1) * span
  cummax(y) - (g - 1) * span + lo
}
