#' Per-patient states over time
#'
#' The data behind [plot_lasagna()]: one row per patient and step with the
#' time since the patient's first step, in the journey's unit (days or
#' months). Patients are sorted by pathway (states dictionary order at step
#' 1, then step 2, ...; then by id) and numbered `row = 1, 2, ...`.
#'
#' For large cohorts only every `k`-th patient of that order is kept, with
#' `k = ceiling(N / n_max)`: a systematic sample that keeps the pathway
#' proportions and is the same in memory and in a database. The ranking and
#' the sampling run with the same `dplyr` code on both (see [engine]); only
#' the sampled patients' steps are collected.
#'
#' @inheritParams jn_flows
#' @param n_max Largest number of patients kept.
#' @param horizon `NULL` (all) or the last time (in units since the first
#'   step) to keep; later parts are cut.
#' @return A data frame with columns `row`, `patient_id`, `step`, `state`
#'   (factor), `from` and `to` (time since the first step; `to` is
#'   exclusive). The end state runs from the end of the last step to the
#'   horizon (the largest `to` when `horizon = NULL`). Attributes:
#'   `n_persons` (all patients), `n_shown` and `every` (`k`).
#' @examples
#' j <- jn_sequence(jn_simulate(500), max_steps = 4)
#' g <- jn_grid(j, n_max = 100)
#' head(g)
#' attr(g, "every")
#' @export
jn_grid <- function(j, n_max = 2000, horizon = NULL,
                    engine = c("auto", "db", "memory"), memory_limit = NULL) {
  check_journey(j)
  if (!is.numeric(n_max) || length(n_max) != 1 || is.na(n_max) || n_max < 1) {
    cli::cli_abort("{.arg n_max} must be a single number >= 1.")
  }
  s <- journey_data(j, engine, memory_limit, "jn_grid")
  grid_from(s, j, n_max, horizon)
}

grid_from <- function(s, j, n_max, horizon) {
  lv <- j@states$level
  s <- dplyr::ungroup(s)
  D <- max_step(s)
  N <- n_persons_of(s)
  cases <- lapply(seq_along(lv), function(k) rlang::expr(state == !!lv[k] ~ !!k))
  coded <- dplyr::mutate(s, code = dplyr::case_when(!!!cases))
  wide <- dplyr::select(dplyr::filter(coded, step == 1L), "patient_id")
  cols <- paste0("c", seq_len(D))
  for (k in seq_len(D)) {
    one <- dplyr::select(dplyr::filter(coded, step == !!k), "patient_id", !!cols[k] := "code")
    wide <- dplyr::left_join(wide, one, by = "patient_id")
  }
  every <- max(1L, as.integer(ceiling(N / n_max)))
  ord <- rlang::syms(c(cols, "patient_id"))
  wide <- win_order(wide, !!!ord)
  wide <- dplyr::mutate(wide, rank = dplyr::row_number())
  wide <- dplyr::filter(wide, (rank - 1L) %% !!every == 0L)
  wide <- dplyr::select(wide, "patient_id", "rank")
  g <- dplyr::inner_join(s, wide, by = "patient_id")
  g <- as.data.frame(dplyr::collect(g), stringsAsFactors = FALSE)

  # small data from here: time since the first step ---------------------------
  unit <- j@meta$unit %||% "day"
  tix <- function(d) if (unit == "month") year(d) * 12 + month(d) - 1 else as.numeric(d)
  g <- g[order(g$rank, g$step), ]
  first <- g$step == 1L
  origin <- tix(g$start[first])[match(g$rank, g$rank[first])]
  g$from <- tix(g$start) - origin
  g$to <- tix(g$end) - origin + 1
  is_end <- !is.null(j@meta$end_state) & g$state == (j@meta$end_state %||% "")
  hz <- horizon %||% suppressWarnings(max(c(g$to[!is_end], g$from + 1), na.rm = TRUE))
  g$from[is_end] <- g$from[is_end] + 1
  g$to[is_end | is.na(g$to)] <- hz
  g <- g[g$from < hz, ]
  g$to <- pmin(g$to, hz)
  g$row <- match(g$rank, sort(unique(g$rank)))
  out <- data.frame(row = g$row, patient_id = g$patient_id, step = g$step,
                    state = factor(g$state, levels = lv), from = g$from, to = g$to,
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  attr(out, "n_persons") <- N
  attr(out, "n_shown") <- length(unique(out$row))
  attr(out, "every") <- every
  attr(out, "unit") <- unit
  out
}

#' Lasagna plot
#'
#' Draw each patient as a horizontal strip coloured by the state at each
#' time since the first step (rows = patients sorted by pathway, columns =
#' days or months). Large cohorts are down-sampled to `n_max` patients by
#' [jn_grid()]. Colours come from the states dictionary
#' ([scale_fill_journey()]).
#'
#' @inheritParams jn_grid
#' @param show_end Draw the end state after the last step (`FALSE` leaves
#'   it blank).
#' @param legend Show the fill legend.
#' @return A `ggplot`.
#' @examples
#' j <- jn_sequence(jn_simulate(500), max_steps = 4)
#' plot_lasagna(j, n_max = 200, horizon = 730)
#' @export
plot_lasagna <- function(j, n_max = 2000, horizon = NULL, show_end = TRUE,
                         legend = TRUE) {
  check_journey(j)
  s <- journey_data(j, "auto", NULL, "plot_lasagna")
  g <- grid_from(s, j, n_max, horizon)
  if (!show_end && !is.null(j@meta$end_state)) g <- g[g$state != j@meta$end_state, ]
  unit <- attr(g, "unit")
  sub <- sprintf("N = %s patients", format(attr(g, "n_persons"), big.mark = ","))
  if (attr(g, "every") > 1) {
    sub <- sprintf("%s; %s shown (every %d%s, sorted by pathway)", sub,
                   format(attr(g, "n_shown"), big.mark = ","), attr(g, "every"),
                   ordinal_suffix(attr(g, "every")))
  }
  ggplot2::ggplot(g) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$from, xmax = .data$to,
                                    ymin = -.data$row, ymax = -.data$row + 1,
                                    fill = .data$state)) +
    legend_keys_layer(j@states, x = 0, y = 0) +
    scale_fill_journey(j) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = paste0(if (unit == "month") "Months" else "Days", " since the first step"),
                  y = NULL, subtitle = sub) +
    theme_journey() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(colour = "grey30"),
                   axis.title.x = ggplot2::element_text(colour = "grey30", margin = ggplot2::margin(t = 4)),
                   legend.position = if (legend) "right" else "none")
}

ordinal_suffix <- function(k) {
  if (k %% 100 %in% 11:13) return("th")
  switch(as.character(k %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
}
