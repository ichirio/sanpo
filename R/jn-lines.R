#' Lines of therapy
#'
#' Build a journey whose steps are lines of therapy and whose states are
#' regimens (the drugs of a line joined by `" + "`). The rules are common
#' simple ones; their defaults are provisional (see `DECISIONS.md`):
#'
#' 1. Rows before the patient's index date are dropped. The index date is
#'    the first `start` of the patient, or the date in column `index`.
#' 2. Rows of the same drug whose gap is at most `grace` form one drug
#'    episode (the gap is measured from the latest `end` of that drug).
#' 3. Drug episodes that start within `window` of the previous episode
#'    start (chained) start one line together.
#' 4. The regimen of a line is the drugs that start in it, plus the drugs
#'    of earlier episodes still taken `window` after the line's last start
#'    (an add-on). A drug that stops within `window` of the new start is
#'    switched away from and is not part of the regimen.
#' 5. A new line therefore starts when a drug is added or switched to.
#'    Consecutive lines with the same regimen (for example a restart of the
#'    same drugs after a gap longer than `grace`) are one line.
#' 6. Lines are cut at `max_steps`; a patient with fewer lines gets
#'    `end_state` after the last one.
#'
#' The same `dplyr` code runs on a data frame and on a lazy database table
#' (see [engine]). A regimen is coded inside the computation as the sum of
#' one bit per drug, so at most 30 drugs (or drug classes) can be used.
#'
#' @param data A data frame or a lazy table of prescription rows.
#' @param id,start,end,drug Column names (strings) in `data`. `drug` holds
#'   the drug or drug class (bring your own code-to-class map).
#' @param index `NULL` (the first `start` of each patient) or the name of a
#'   `Date` column holding each patient's index date.
#' @param unit `"day"` or `"month"`, as in [jn_sequence()].
#' @param window Days (months) after a line start within which drugs that
#'   start count as one regimen. Defaults to 28 days or 0 months.
#' @param grace Largest gap of a drug that still counts as continuous.
#'   Defaults to 60 days or 2 months.
#' @param drugs `NULL` or a character vector of drugs, in the order used
#'   inside regimen names. `NULL` uses the levels of `data[[drug]]` when it
#'   is a factor, otherwise its sorted unique values.
#' @param states `NULL` or a [jn_states()] dictionary / character vector of
#'   regimen names (the node order). `NULL` uses every single drug (unused
#'   ones are empty nodes), then the combinations seen, ordered by number of
#'   drugs and then by `drugs`.
#' @param sep Separator between drugs in regimen names.
#' @inheritParams jn_sequence
#' @return A [journey()] with one step per line; `meta$type` is `"lines"`.
#' @examples
#' ep <- jn_simulate(300)
#' lt <- jn_lines(ep, drug = "state", max_steps = 3)
#' lt
#' tbl_pathways(lt)
#' @export
jn_lines <- function(data, id = "patient_id", start = "start", end = "end",
                     drug = "drug", index = NULL, unit = c("day", "month"),
                     window = NULL, grace = NULL, max_steps = 4,
                     end_state = "(end)", drugs = NULL, states = NULL,
                     sep = " + ", engine = c("auto", "db", "memory"),
                     memory_limit = NULL) {
  unit <- match.arg(unit)
  engine <- match.arg(engine)
  if (!is.data.frame(data) && !is_lazy(data)) {
    cli::cli_abort("{.arg data} must be a data frame or a lazy database table.")
  }
  proto <- col_proto(data)
  miss <- setdiff(c(id, start, end, drug, index), names(proto))
  if (length(miss) > 0) cli::cli_abort("Column{?s} {.val {miss}} not found in {.arg data}.")
  if (is.null(window)) window <- if (unit == "day") 28 else 0
  if (is.null(grace)) grace <- if (unit == "day") 60 else 2
  for (a in c("window", "grace")) {
    v <- get(a)
    if (!is.numeric(v) || length(v) != 1 || is.na(v) || v < 0) {
      cli::cli_abort("{.arg {a}} must be a single number >= 0.")
    }
  }
  if (!is.numeric(max_steps) || length(max_steps) != 1 || is.na(max_steps) || max_steps < 1) {
    cli::cli_abort("{.arg max_steps} must be a single number >= 1.")
  }
  for (a in c(start, end, index)) {
    if (!inherits(proto[[a]], "Date")) cli::cli_abort("Column {.field {a}} must be a {.cls Date}.")
  }
  if (is.null(drugs) && is.factor(proto[[drug]])) drugs <- levels(proto[[drug]])

  ch <- choose_engine(data, engine, memory_limit, "jn_lines")
  x <- dplyr::select(ch$data, patient_id = dplyr::all_of(id),
                     state = dplyr::all_of(drug), start = dplyr::all_of(start),
                     end = dplyr::all_of(end), dplyr::all_of(index))
  x <- dplyr::mutate(x, state = as.character(state))
  if (is.null(drugs)) drugs <- sort(distinct_values(x, "state"))
  drugs <- as.character(drugs)
  if (length(drugs) > 30) cli::cli_abort("{.fn jn_lines} handles at most 30 drugs (or drug classes).")
  bad <- setdiff(distinct_values(x, "state"), drugs)
  if (length(bad) > 0) cli::cli_abort("Drug{?s} {.val {bad}} {?is/are} not in {.arg drugs}.")
  if (count_where(x, is.na(patient_id) | is.na(state) | is.na(start)) > 0) {
    cli::cli_abort("{.field {id}}, {.field {drug}} and {.field {start}} must not be missing.")
  }

  # 1. index date ----------------------------------------------------------
  if (!is.null(index)) {
    x <- dplyr::filter(x, start >= .data[[index]])
    x <- dplyr::select(x, -dplyr::all_of(index))
  }
  x <- add_time_index(x, unit, end)

  # 2. drug episodes ------------------------------------------------------
  # windows partitioned per patient in a database, none in memory (see
  # steps_from_index())
  x <- win_order(win_part(x), patient_id, state, t0, t1)
  x <- mark_blocks(x, c("patient_id", "state"), "new_pd", "pd")
  x <- block_cummax(x, "run_end", "t1", "pd")
  x <- dplyr::mutate(x, prev_end = dplyr::lag(run_end))
  x <- dplyr::mutate(x, new_run = dplyr::if_else(
    new_pd == 0L & t0 - prev_end - 1L <= !!grace, 0L, 1L
  ))
  ep <- collapse_blocks(x, "new_run", first = c(e0 = "t0"), max = c(e1 = "t1"))
  ep <- checkpoint(dplyr::select(ep, "patient_id", "state", "e0", "e1"))

  # 3. line starts: episode starts chained within `window` -----------------
  ep <- win_order(win_part(ep), patient_id, e0, state)
  ep <- mark_blocks(ep, "patient_id", "new_p", "p")
  ep <- dplyr::mutate(ep, prev_e0 = dplyr::lag(e0))
  ep <- dplyr::mutate(ep, new_line = dplyr::if_else(
    new_p == 0L & e0 - prev_e0 <= !!window, 0L, 1L
  ))
  ep <- checkpoint(dplyr::ungroup(dplyr::mutate(ep, line = cumsum(new_line))))
  lines <- win_order(win_part(ep), patient_id, e0, state)
  lines <- collapse_blocks(lines, "new_line", first = c(c0 = "e0"),
                           max = c(c_last = "e0"), keep = "patient_id")
  lines <- dplyr::select(lines, "patient_id", line = "block", "c0", "c_last")
  ep <- dplyr::select(ep, "patient_id", "state", "e0", "e1")

  # 4. regimen: drugs starting in the line + earlier drugs still taken -----
  bits <- stats::setNames(as.list(2^(seq_along(drugs) - 1)), drugs)
  bit_cases <- lapply(seq_along(drugs), function(k) {
    rlang::expr(state == !!drugs[k] ~ !!bits[[k]])
  })
  ep <- dplyr::mutate(ep, bit = dplyr::case_when(!!!bit_cases))
  ep <- dplyr::select(ep, "patient_id", "bit", "e0", "e1")
  mem <- dplyr::inner_join(lines, ep, by = "patient_id", relationship = "many-to-many")
  mem <- checkpoint(mem)
  mem <- dplyr::filter(mem, (e0 >= c0 & e0 <= c_last) |
                         (e0 < c0 & e1 >= c_last + !!window))
  reg <- dplyr::distinct(mem, patient_id, line, c0, bit)
  reg <- dplyr::summarise(dplyr::group_by(reg, patient_id, line, c0),
                          code = sum(bit, na.rm = TRUE), .groups = "drop")
  ends <- dplyr::summarise(dplyr::group_by(mem, patient_id, line),
                           t1 = max(e1, na.rm = TRUE), .groups = "drop")
  reg <- dplyr::inner_join(reg, ends, by = c("patient_id", "line"))

  # regimen names from the codes seen (a small query) ----------------------
  codes <- sort(as.numeric(distinct_values(reg, "code")))
  names_seen <- vapply(codes, regimen_name, character(1), drugs = drugs, sep = sep)
  if (is.null(states)) {
    # every single drug (unused ones are empty nodes), then the
    # combinations seen, by number of drugs and drug order
    all_codes <- sort(unique(c(2^(seq_along(drugs) - 1), codes)))
    n_drugs <- vapply(all_codes, function(cd) sum(code_bits(cd, length(drugs))), numeric(1))
    key <- vapply(all_codes, function(cd) paste(sprintf("%02d", which(code_bits(cd, length(drugs)) == 1)), collapse = ""), character(1))
    all_names <- vapply(all_codes, regimen_name, character(1), drugs = drugs, sep = sep)
    states <- jn_states(all_names[order(n_drugs, key)])
  } else {
    states <- as_jn_states(states)
  }
  if (!is.null(end_state) && !end_state %in% states$level) {
    lv <- c(states$level, end_state)
    states <- jn_states(lv, c(states$label, end_state),
                        c(states$colour, jn_default_colours(lv)[length(lv)]))
  }
  miss <- setdiff(names_seen, states$level)
  if (length(miss) > 0) cli::cli_abort("Regimen{?s} {.val {miss}} {?is/are} not in {.arg states}.")
  name_cases <- lapply(seq_along(codes), function(k) {
    rlang::expr(code == !!codes[k] ~ !!names_seen[k])
  })
  reg <- dplyr::mutate(reg, state = dplyr::case_when(!!!name_cases), t0 = c0)
  reg <- dplyr::select(reg, "patient_id", "state", "t0", "t1")

  # 5-6. steps: same regimen in a row is one line ---------------------------
  seq <- steps_from_index(reg, unit, grace = 0, gap_state = NULL,
                          collapse = TRUE, max_steps = max_steps,
                          end_state = end_state)
  meta <- list(type = "lines", unit = unit, index = index %||% "first start",
               window = window, grace = grace, max_steps = max_steps,
               end_state = end_state)
  finish_sequence(seq, ch$engine, engine, memory_limit, states, meta, "jn_lines")
}

code_bits <- function(code, k) as.integer(intToBits(as.integer(code))[seq_len(k)])

regimen_name <- function(code, drugs, sep) {
  paste(drugs[code_bits(code, length(drugs)) == 1], collapse = sep)
}
