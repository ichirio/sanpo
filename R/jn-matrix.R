#' Transition matrix between consecutive steps
#'
#' Count the patients moving from each state to each state between
#' consecutive steps, as a long data frame with every pair of levels (zeros
#' included). By default the step pairs are pooled (one matrix); with
#' `by_step = TRUE` there is one matrix per step pair. The end state has no
#' outflows and is left out of `from`.
#'
#' @inheritParams jn_flows
#' @param by_step One matrix per step pair (`TRUE`) or pooled (`FALSE`).
#' @param min_cell Suppress counts from 1 to `min_cell - 1`; see
#'   [jn_suppress()].
#' @return A data frame with columns `from`, `to` (factors with the
#'   journey's levels), `n`, `pct` (share of the row: of the patients
#'   leaving `from`), `n_label`, and `step_from`, `step_to` when
#'   `by_step = TRUE`.
#' @examples
#' j <- jn_sequence(jn_simulate(300), max_steps = 4)
#' m <- jn_matrix(j)
#' m[m$n > 0, ]
#' @export
jn_matrix <- function(j, steps = NULL, by_step = FALSE, min_cell = NULL,
                      engine = c("auto", "db", "memory"), memory_limit = NULL) {
  check_journey(j)
  s <- journey_data(j, engine, memory_limit, "jn_matrix")
  matrix_from(s, j, steps, by_step, min_cell)
}

matrix_from <- function(s, j, steps, by_step, min_cell) {
  lv <- j@states$level
  fl <- flows_from(s, lv, steps)
  from_lv <- setdiff(lv, j@meta$end_state)
  fl <- fl[as.character(fl$from) %in% from_lv, ]
  if (by_step) {
    out <- fl[, c("step_from", "step_to", "from", "to", "n")]
    tot <- stats::ave(out$n, out$step_from, out$from, FUN = sum)
  } else {
    key <- paste(as.integer(fl$from), as.integer(fl$to))
    n <- tapply(fl$n, factor(key, levels = unique(key)), sum)
    first <- match(names(n), key)
    out <- data.frame(from = fl$from[first], to = fl$to[first], n = as.integer(n))
    if (nrow(out) == 0) {
      out <- data.frame(from = factor(character(), levels = lv),
                        to = factor(character(), levels = lv), n = integer())
    }
    tot <- stats::ave(out$n, out$from, FUN = sum)
  }
  out$pct <- ifelse(tot > 0, out$n / tot, 0)
  rownames(out) <- NULL
  apply_suppression(out, min_cell, cols = "n", pct = "pct")
}

#' Transition matrix plot
#'
#' Draw [jn_matrix()] as tiles: rows are the state at one step, columns the
#' state at the next, the fill is the row percentage and the text the count
#' (and percentage). Rows and columns follow the states dictionary and keep
#' empty states; a coloured key in the states' colours
#' ([scale_colour_journey()]) marks each row and column.
#'
#' @inheritParams jn_matrix
#' @param label Text in the tiles: `"n_pct"`, `"n"`, `"pct"` or `"none"`.
#' @param label_size Text size.
#' @param high Colour of 100% in the fill gradient.
#' @return A `ggplot`.
#' @examples
#' j <- jn_sequence(jn_simulate(300), max_steps = 4)
#' plot_transition(j)
#' plot_transition(j, by_step = TRUE, label = "pct")
#' @export
plot_transition <- function(j, steps = NULL, by_step = FALSE,
                            label = c("n_pct", "n", "pct", "none"),
                            min_cell = NULL, label_size = 3, high = "#0072B2") {
  check_journey(j)
  label <- match.arg(label)
  s <- journey_data(j, "auto", NULL, "plot_transition")
  m <- matrix_from(s, j, steps, by_step, min_cell)
  st <- j@states
  from_lv <- setdiff(st$level, j@meta$end_state)
  m$x <- as.integer(m$to)
  m$y <- match(as.character(m$from), rev(from_lv))
  m$text <- if (label == "none") "" else {
    txt <- format_count_label(m$n, m$pct, label)
    if ("suppressed" %in% names(m)) txt[m$suppressed] <- m$n_label[m$suppressed]
    txt
  }
  m$text[!is.na(m$n) & m$n == 0] <- ""
  if (by_step) m$pair <- factor(paste("Step", m$step_from, "->", m$step_to),
                                levels = unique(paste("Step", m$step_from, "->", m$step_to)))
  keys_x <- data.frame(state = factor(st$level, levels = st$level),
                       x = seq_along(st$level), y = 0.25)
  keys_y <- data.frame(state = factor(from_lv, levels = st$level),
                       x = 0.25, y = rev(seq_along(from_lv)))
  p <- ggplot2::ggplot(m) +
    ggplot2::geom_tile(ggplot2::aes(x = .data$x, y = .data$y, fill = .data$pct),
                       colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(x = .data$x, y = .data$y, label = .data$text),
                       size = label_size, colour = "grey10") +
    ggplot2::geom_point(data = keys_x, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$state),
                        shape = 15, size = 4) +
    ggplot2::geom_point(data = keys_y, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$state),
                        shape = 15, size = 4) +
    ggplot2::scale_fill_gradient(name = "Row %", low = "grey97", high = high,
                                 limits = c(0, 1), labels = function(v) sprintf("%.0f%%", 100 * v),
                                 na.value = "grey90") +
    scale_colour_journey(st, guide = "none") +
    ggplot2::scale_x_continuous(breaks = seq_along(st$level), labels = st$label,
                                position = "top", expand = ggplot2::expansion(add = 0.1)) +
    ggplot2::scale_y_continuous(breaks = seq_along(from_lv), labels = rev(st$label[match(from_lv, st$level)]),
                                expand = ggplot2::expansion(add = 0.1)) +
    ggplot2::labs(x = "To (next step)", y = "From",
                  subtitle = sprintf("N = %s patients", format(n_persons_of(s), big.mark = ","))) +
    theme_journey() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(colour = "grey30", hjust = 1),
                   axis.title = ggplot2::element_text(colour = "grey30"),
                   axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = 6)),
                   panel.grid = ggplot2::element_blank())
  if (by_step) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$pair))
  }
  p
}
