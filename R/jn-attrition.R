#' Cohort attrition
#'
#' Apply inclusion criteria one after another and count the patients left
#' and excluded at each step. Each criterion is a `dplyr::filter()`
#' condition on the rows of `data`; a patient remains while at least one of
#' their rows passes every criterion so far. The counts are made with the
#' same `dplyr` code in memory and in a database (see [engine]); only the
#' counts are collected.
#'
#' @param data A data frame or a lazy table (one or more rows per patient).
#' @param ... Named conditions, in order, e.g.
#'   `"Drug A or B" = state %in% c("Drug A", "Drug B")`. The name is the
#'   label of the step.
#' @param id Patient id column (string).
#' @param start_label Label of the first box (all patients).
#' @param min_cell Suppress counts from 1 to `min_cell - 1`; see
#'   [jn_suppress()].
#' @inheritParams jn_sequence
#' @return A data frame of class `jn_attrition` with columns `step` (0 =
#'   all patients), `criterion`, `n` (patients remaining), `excluded`
#'   (patients removed by this step), `pct` (share of the first count),
#'   `n_label`, `excluded_label`.
#' @examples
#' ep <- jn_simulate(500)
#' at <- jn_attrition(ep,
#'   "Started in 2022" = start < as.Date("2023-01-01"),
#'   "Drug A to C" = state %in% c("Drug A", "Drug B", "Drug C"),
#'   "Supply >= 28 days" = end - start >= 27
#' )
#' at
#' plot_attrition(at)
#' @export
jn_attrition <- function(data, ..., id = "patient_id", start_label = "All patients",
                         min_cell = NULL, engine = c("auto", "db", "memory"),
                         memory_limit = NULL) {
  engine <- match.arg(engine)
  if (!is.data.frame(data) && !is_lazy(data)) {
    cli::cli_abort("{.arg data} must be a data frame or a lazy database table.")
  }
  crit <- rlang::enquos(...)
  if (length(crit) == 0) cli::cli_abort("Give at least one criterion in {.arg ...}.")
  if (is.null(names(crit)) || any(names(crit) == "")) {
    cli::cli_abort("Every criterion in {.arg ...} needs a name (its label).")
  }
  if (!id %in% names(col_proto(data))) cli::cli_abort("Column {.val {id}} not found in {.arg data}.")
  if (engine == "db" && !is_lazy(data)) engine <- "memory"
  x <- choose_engine(data, engine, memory_limit, "jn_attrition")$data
  n_ids <- function(d) count_rows(dplyr::distinct(dplyr::ungroup(d), dplyr::across(dplyr::all_of(id))))
  n <- numeric(length(crit) + 1)
  n[1] <- n_ids(x)
  for (i in seq_along(crit)) {
    x <- dplyr::filter(x, !!crit[[i]])
    n[i + 1] <- n_ids(x)
  }
  out <- data.frame(step = seq_along(n) - 1L, criterion = c(start_label, names(crit)),
                    n = as.integer(n), excluded = as.integer(c(NA, -diff(n))),
                    stringsAsFactors = FALSE)
  out$pct <- if (n[1] > 0) out$n / n[1] else NA_real_
  out <- apply_suppression(out, min_cell, cols = c("n", "excluded"), pct = "pct")
  if (!"n_label" %in% names(out)) {
    out$n_label <- format_n(out$n)
    out$excluded_label <- format_n(out$excluded)
  }
  class(out) <- c("jn_attrition", "data.frame")
  out
}

format_n <- function(n) ifelse(is.na(n), "", format(n, big.mark = ",", trim = TRUE))

#' Attrition flow chart
#'
#' Draw [jn_attrition()] as a flow chart with `ggplot2`: one box per step
#' with the patients remaining, arrows down the middle and the patients
#' excluded at each step in boxes to the right.
#'
#' @param x A [jn_attrition()] result.
#' @param box_fill,excl_fill Fill of the main and the exclusion boxes.
#' @param text_size Text size.
#' @param wrap Wrap labels at this many characters.
#' @return A `ggplot`.
#' @examples
#' at <- jn_attrition(jn_simulate(300), "Drug A or B" = state %in% c("Drug A", "Drug B"))
#' plot_attrition(at)
#' @export
plot_attrition <- function(x, box_fill = "grey95", excl_fill = "white",
                           text_size = 3.2, wrap = 30) {
  if (!inherits(x, "jn_attrition")) cli::cli_abort("{.arg x} must come from {.fn jn_attrition}.")
  k <- nrow(x)
  y <- -(seq_len(k) - 1) * 2
  wrapl <- function(s) vapply(strwrap(s, wrap, simplify = FALSE), paste, character(1), collapse = "\n")
  main <- data.frame(
    x = 0, y = y,
    text = sprintf("%s\nn = %s", wrapl(x$criterion), x$n_label)
  )
  ex <- x[-1, , drop = FALSE]
  excl <- data.frame(x = 1.6, y = y[-1] + 1,
                     text = sprintf("Excluded\nn = %s", ex$excluded_label))
  arrows <- data.frame(x = 0, xend = 0, y = y[-k] - 0.45, yend = y[-1] + 0.45)
  side <- data.frame(x = 0, xend = 1.6 - 0.62, y = y[-1] + 1, yend = y[-1] + 1)
  bw <- 0.62
  bh <- 0.45
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = arrows,
                          ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm"), type = "closed"),
                          colour = "grey30") +
    ggplot2::geom_segment(data = side,
                          ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm"), type = "closed"),
                          colour = "grey30") +
    ggplot2::geom_rect(data = main,
                       ggplot2::aes(xmin = .data$x - bw, xmax = .data$x + bw,
                                    ymin = .data$y - bh, ymax = .data$y + bh),
                       fill = box_fill, colour = "grey30", linewidth = 0.3) +
    ggplot2::geom_rect(data = excl,
                       ggplot2::aes(xmin = .data$x - bw, xmax = .data$x + bw,
                                    ymin = .data$y - bh * 0.8, ymax = .data$y + bh * 0.8),
                       fill = excl_fill, colour = "grey30", linewidth = 0.3) +
    ggplot2::geom_text(data = main, ggplot2::aes(x = .data$x, y = .data$y, label = .data$text),
                       size = text_size, lineheight = 0.95) +
    ggplot2::geom_text(data = excl, ggplot2::aes(x = .data$x, y = .data$y, label = .data$text),
                       size = text_size * 0.9, lineheight = 0.95, colour = "grey20") +
    ggplot2::coord_cartesian(xlim = c(-0.7, 2.3), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10))
}
