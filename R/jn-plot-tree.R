#' Sunburst and icicle of a journey
#'
#' Both plots draw the same rectangles from [jn_tree()]: one ring (sunburst)
#' or one column (icicle) per step, each prefix of the pathways as a segment
#' whose size is its share of all patients, children ordered by the states
#' dictionary inside their parent. The sunburst is the icicle in
#' [ggplot2::coord_radial()].
#'
#' @param j A [journey()].
#' @param steps Number of steps (rings or columns) to show; a vector uses
#'   its maximum. `NULL` = all.
#' @param label Text on the segments: `"pct"` (share of all patients),
#'   `"n"`, `"n_pct"`, `"state"` or `"none"`.
#' @param label_min Segments smaller than this share of all patients are not
#'   labelled.
#' @param label_size Text size.
#' @param inner_radius Size of the hole of the sunburst, from 0 to 1.
#' @param legend Show the fill legend.
#' @return A `ggplot`.
#' @examples
#' j <- jn_sequence(jn_simulate(500), max_steps = 3)
#' plot_sunburst(j)
#' plot_icicle(j)
#' @name plot_tree
NULL

#' @rdname plot_tree
#' @export
plot_sunburst <- function(j, steps = NULL, label = c("pct", "n", "n_pct", "state", "none"),
                          label_min = 0.03, label_size = 3, inner_radius = 0.25,
                          legend = TRUE) {
  check_journey(j)
  label <- match.arg(label)
  tr <- jn_tree(j, depth = steps)
  tr$text <- tree_label(tr, j, label, label_min)
  N <- j$meta$n_persons
  D <- max(tr$depth)
  ggplot2::ggplot(tr) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data$start, xmax = .data$end,
                   ymin = .data$depth - 1, ymax = .data$depth, fill = .data$state),
      colour = "white", linewidth = 0.3
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = (.data$start + .data$end) / 2, y = .data$depth - 0.5,
                   label = .data$text),
      size = label_size, colour = "grey10"
    ) +
    ggplot2::coord_radial(theta = "x", expand = FALSE, inner.radius = inner_radius) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, D)) +
    legend_keys_layer(j$states) +
    scale_fill_journey(j) +
    ggplot2::labs(subtitle = sprintf("N = %s patients; rings = steps 1 to %d (inside out)",
                                     format(N, big.mark = ","), D)) +
    theme_journey() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   legend.position = if (legend) "right" else "none")
}

#' @rdname plot_tree
#' @export
plot_icicle <- function(j, steps = NULL, label = c("n_pct", "pct", "n", "state", "none"),
                        label_min = 0.03, label_size = 3, legend = TRUE) {
  check_journey(j)
  label <- match.arg(label)
  tr <- jn_tree(j, depth = steps)
  tr$text <- tree_label(tr, j, label, label_min)
  N <- j$meta$n_persons
  D <- max(tr$depth)
  ggplot2::ggplot(tr) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data$depth - 1, xmax = .data$depth,
                   ymin = 1 - .data$end, ymax = 1 - .data$start, fill = .data$state),
      colour = "white", linewidth = 0.3
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = .data$depth - 0.5, y = 1 - (.data$start + .data$end) / 2,
                   label = .data$text),
      size = label_size, colour = "grey10"
    ) +
    ggplot2::scale_x_continuous(breaks = seq_len(D) - 0.5,
                                labels = paste("Step", seq_len(D)),
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    legend_keys_layer(j$states) +
    scale_fill_journey(j) +
    ggplot2::labs(x = NULL, y = NULL,
                  subtitle = sprintf("N = %s patients", format(N, big.mark = ","))) +
    theme_journey() +
    ggplot2::theme(legend.position = if (legend) "right" else "none")
}

tree_label <- function(tr, j, label, label_min) {
  txt <- if (label == "state") {
    j$states$label[as.integer(tr$state)]
  } else {
    format_count_label(tr$n, tr$pct, label)
  }
  txt[tr$pct < label_min] <- ""
  txt
}
