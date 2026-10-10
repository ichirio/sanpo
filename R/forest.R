#' Forest plot of ratios with confidence intervals
#'
#' * `sp_forest_data()` takes one row per estimate (a rate ratio, odds ratio,
#'   hazard ratio, ...) with its confidence limits, and adds the text column
#'   (`"1.23 (1.05-1.44)"`) and the row order. Column names from
#'   `broom::tidy()` (`term`, `conf.low`, `conf.high`) are recognised when
#'   the default names are not found.
#' * `sp_plot_forest()` draws the estimates on a log axis with a reference
#'   line at 1 and the estimates as text on the right-hand axis. Rows can be
#'   grouped under headings (`group`).
#' * `sp_forest()` is the two in one call.
#'
#' @param data Data frame with one row per estimate.
#' @param label Row label column.
#' @param estimate,lower,upper Estimate and confidence-limit columns.
#' @param group Optional heading column: rows are shown in blocks under it.
#' @param digits Decimal places of the text column.
#' @return `sp_forest_data()`: a data frame with `label`, `estimate`,
#'   `lower`, `upper`, `group` (if given), `text` and `row` (1 = top).
#' @examples
#' est <- sp_synth_estimates()
#' est
#' sp_forest(est, group = "group")
#' sp_forest(est, log_scale = FALSE, xlab = "Odds ratio (95% CI)")
#' @export
sp_forest_data <- function(data, label = "label", estimate = "estimate", lower = "lower",
                           upper = "upper", group = NULL, digits = 2) {
  sp_check_data(data)
  # recognise broom::tidy() names when the defaults are absent
  if (identical(label, "label") && !"label" %in% names(data) && "term" %in% names(data)) label <- "term"
  if (identical(lower, "lower") && !"lower" %in% names(data) && "conf.low" %in% names(data)) lower <- "conf.low"
  if (identical(upper, "upper") && !"upper" %in% names(data) && "conf.high" %in% names(data)) upper <- "conf.high"
  sp_check_cols(data, list(label = label, estimate = estimate, lower = lower, upper = upper,
                           group = group))
  for (a in c("estimate", "lower", "upper")) {
    sp_check_numeric_col(data, get(a), a)
  }
  est <- data[[estimate]]
  lo <- data[[lower]]
  hi <- data[[upper]]
  if (any(lo > est | est > hi)) {
    stop("Each row must have `lower` <= `estimate` <= `upper`; rows ",
         paste(utils::head(which(lo > est | est > hi), 5), collapse = ", "), " do not.",
         call. = FALSE)
  }
  out <- data.frame(label = as.character(data[[label]]), estimate = est, lower = lo, upper = hi)
  if (!is.null(group)) {
    g <- data[[group]]
    out$group <- if (is.factor(g)) g else factor(g, levels = unique(g))
    out <- out[order(out$group, seq_len(nrow(out))), , drop = FALSE]
  }
  fmt <- function(x) formatC(x, format = "f", digits = digits)
  out$text <- paste0(fmt(out$estimate), " (", fmt(out$lower), "-", fmt(out$upper), ")")
  out$row <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

#' @rdname sp_forest_data
#' @param forest A table from `sp_forest_data()`.
#' @param log_scale Use a log x axis (for ratios).
#' @param ref Reference line position (`NULL` for none).
#' @param xlab X-axis title. Default: `"Estimate (95% CI)"`.
#' @param show_text Show the estimate text on the right-hand axis.
#' @param colour Colour of the points and lines.
#' @param lang Label language, `"en"` or `"ja"`.
#' @export
sp_plot_forest <- function(forest, log_scale = TRUE, ref = 1, xlab = NULL, show_text = TRUE,
                           colour = "grey15", lang = getOption("sanpo.lang", "en")) {
  lang <- sp_lang(lang)
  sp_check_data(forest, "forest")
  sp_check_cols(forest, list(label = "label", estimate = "estimate", lower = "lower",
                             upper = "upper", text = "text", row = "row"), "forest")
  if (log_scale && any(forest$lower <= 0)) {
    stop("A log axis needs positive estimates and limits; use `log_scale = FALSE`.", call. = FALSE)
  }
  forest$y <- -forest$row
  labs_by_y <- stats::setNames(forest$label, forest$y)
  text_by_y <- stats::setNames(forest$text, forest$y)
  p <- ggplot2::ggplot(forest, ggplot2::aes(x = .data$estimate, y = .data$y))
  if (!is.null(ref)) {
    p <- p + ggplot2::geom_vline(xintercept = ref, colour = "grey50", linewidth = 0.4,
                                 linetype = "dashed")
  }
  p <- p +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
                           width = 0, linewidth = 0.5, colour = colour, orientation = "y") +
    ggplot2::geom_point(shape = 15, size = 2.2, colour = colour) +
    ggplot2::scale_y_continuous(
      breaks = forest$y, labels = function(b) unname(labs_by_y[as.character(b)]),
      expand = ggplot2::expansion(add = 0.6),
      sec.axis = if (show_text) {
        ggplot2::dup_axis(labels = function(b) unname(text_by_y[as.character(b)]), name = NULL)
      } else {
        ggplot2::waiver()
      }
    ) +
    ggplot2::labs(x = xlab %or% sp_label("estimate", lang), y = NULL) +
    sp_theme(lang = lang) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())
  if (log_scale) p <- p + ggplot2::scale_x_log10()
  if (!is.null(forest$group)) {
    p <- p + ggplot2::facet_grid(rows = ggplot2::vars(.data$group), scales = "free_y",
                                 space = "free_y", switch = "y") +
      ggplot2::theme(strip.placement = "outside",
                     strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold",
                                                               hjust = 0, vjust = 1))
  }
  p
}

#' @rdname sp_forest_data
#' @param ... Further arguments passed to `sp_plot_forest()`.
#' @export
sp_forest <- function(data, label = "label", estimate = "estimate", lower = "lower",
                      upper = "upper", group = NULL, digits = 2, ...) {
  fo <- sp_forest_data(data, label = label, estimate = estimate, lower = lower, upper = upper,
                       group = group, digits = digits)
  sp_attach(sp_plot_forest(fo, ...), fo)
}
