#' Sankey diagram of a journey
#'
#' Draw the states at each step as stacked nodes and the patients moving
#' between steps as ribbons. Only the aggregated flows from [jn_flows()] are
#' drawn (one polygon per non-empty pair of states), so the cost does not
#' grow with the number of patients.
#'
#' Node order and colours come from the states dictionary, so they match
#' [plot_sunburst()] and [plot_icicle()]. With `empty = "show"`, a state no
#' patient is in at a step is still drawn as a thin node labelled `0`
#' (the end and gap states are not drawn empty).
#'
#' @param j A [journey()].
#' @param steps Steps to show (`NULL` = all).
#' @param label Count shown after each state name: `"n_pct"`, `"n"`,
#'   `"pct"` (share of all patients) or `"none"`.
#' @param empty `"show"` or `"hide"` empty nodes.
#' @param node_width Node width, in steps.
#' @param node_gap Vertical gap between nodes, as a share of all patients.
#' @param flow_alpha Opacity of the ribbons.
#' @param label_size Text size of the node labels.
#' @param legend Show the fill legend (the nodes are labelled already).
#' @return A `ggplot`.
#' @examples
#' j <- jn_sequence(jn_simulate(500), max_steps = 3)
#' plot_sankey(j)
#' @export
plot_sankey <- function(j, steps = NULL, label = c("n_pct", "n", "pct", "none"),
                        empty = c("show", "hide"), node_width = 0.12,
                        node_gap = 0.03, flow_alpha = 0.45, label_size = 3.2,
                        legend = FALSE) {
  check_journey(j)
  label <- match.arg(label)
  empty <- match.arg(empty)
  m <- jn_state_matrix(j)
  steps <- resolve_steps(steps, ncol(m))
  N <- j@n_persons
  st <- j@states
  special <- c(j@meta$end_state, j@meta$gap_state)

  # nodes --------------------------------------------------------------------
  nodes <- jn_nodes(j, steps)
  nodes$x <- match(nodes$step, steps)
  nodes$h <- nodes$n / N
  show <- nodes$n > 0 | (empty == "show" & !(as.character(nodes$state) %in% special))
  nodes <- nodes[show, ]
  min_h <- 0.004
  nodes$h_draw <- pmax(nodes$h, min_h)
  nodes$top <- stats::ave(nodes$h_draw + node_gap, nodes$x,
                          FUN = function(v) -(cumsum(v) - v))
  nodes$bottom <- nodes$top - nodes$h_draw
  nodes$empty <- nodes$n == 0
  nodes$text <- paste0(st$label[as.integer(nodes$state)],
                       ifelse(label == "none", "", "  "),
                       format_count_label(nodes$n, nodes$n / N, label))

  # flows --------------------------------------------------------------------
  fl <- jn_flows(j, steps)
  fl <- fl[fl$n > 0, ]
  ribbons <- NULL
  if (nrow(fl) > 0) {
    fl$x0 <- match(fl$step_from, steps)
    fl$x1 <- fl$x0 + 1
    key_n <- paste(nodes$x, as.integer(nodes$state))
    src_top <- nodes$top[match(paste(fl$x0, as.integer(fl$from)), key_n)]
    tgt_top <- nodes$top[match(paste(fl$x1, as.integer(fl$to)), key_n)]
    # outflows ordered by target level, inflows by source level
    o <- order(fl$x0, fl$from, fl$to)
    fl <- fl[o, ]
    src_top <- src_top[o]
    tgt_top <- tgt_top[o]
    off_src <- stats::ave(fl$n, fl$x0, fl$from, FUN = function(v) cumsum(v) - v)
    o2 <- order(fl$x1, fl$to, fl$from)
    off_tgt <- numeric(nrow(fl))
    off_tgt[o2] <- stats::ave(fl$n[o2], fl$x1[o2], fl$to[o2], FUN = function(v) cumsum(v) - v)
    h <- fl$n / N
    y0t <- src_top - off_src / N
    y1t <- tgt_top - off_tgt / N
    ribbons <- ribbon_polygons(
      x0 = fl$x0 + node_width / 2, x1 = fl$x1 - node_width / 2,
      y0_top = y0t, y0_bot = y0t - h, y1_top = y1t, y1_bot = y1t - h,
      fill = fl$from
    )
  }

  # plot ---------------------------------------------------------------------
  p <- ggplot2::ggplot()
  if (!is.null(ribbons)) {
    p <- p + ggplot2::geom_polygon(
      data = ribbons,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$fill),
      alpha = flow_alpha, colour = NA
    )
  }
  p <- p +
    ggplot2::geom_rect(
      data = nodes,
      ggplot2::aes(xmin = .data$x - node_width / 2, xmax = .data$x + node_width / 2,
                   ymin = .data$bottom, ymax = .data$top, fill = .data$state),
      colour = "grey20", linewidth = 0.25
    ) +
    do.call(ggplot2::geom_label, c(list(
      data = nodes,
      mapping = ggplot2::aes(x = .data$x + node_width / 2 + 0.02,
                             y = (.data$top + .data$bottom) / 2, label = .data$text),
      hjust = 0, size = label_size,
      colour = ifelse(nodes$empty, "grey45", "grey10"),
      fontface = ifelse(nodes$empty, "italic", "plain"),
      fill = grDevices::adjustcolor("white", alpha.f = 0.7),
      label.padding = ggplot2::unit(0.12, "lines")
    ), label_no_border())) +
    legend_keys_layer(st, x = 1, y = 0) +
    scale_fill_journey(st) +
    ggplot2::scale_x_continuous(
      breaks = seq_along(steps), labels = paste("Step", steps),
      expand = ggplot2::expansion(add = c(0.15, 0.95))
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::labs(x = NULL, y = NULL,
                  subtitle = sprintf("N = %s patients", format(N, big.mark = ","))) +
    theme_journey() +
    ggplot2::theme(legend.position = if (legend) "bottom" else "none")
  p
}

# geom_label() without a border: `label.size` before ggplot2 4.0,
# `border.colour` from 4.0 on.
label_no_border <- function() {
  if (utils::packageVersion("ggplot2") >= "4.0.0") {
    list(border.colour = NA)
  } else {
    list(label.size = 0)
  }
}

# One polygon per ribbon: smooth (cubic Hermite) top and bottom edges.
ribbon_polygons <- function(x0, x1, y0_top, y0_bot, y1_top, y1_bot, fill,
                            n_pts = 40) {
  t <- seq(0, 1, length.out = n_pts)
  s <- t * t * (3 - 2 * t)
  k <- length(x0)
  id <- rep(seq_len(k), each = 2 * n_pts)
  tt <- rep(c(t, rev(t)), k)
  ss <- rep(c(s, rev(s)), k)
  top <- rep(c(rep(TRUE, n_pts), rep(FALSE, n_pts)), k)
  xa <- x0[id]
  xb <- x1[id]
  ya <- ifelse(top, y0_top[id], y0_bot[id])
  yb <- ifelse(top, y1_top[id], y1_bot[id])
  data.frame(id = id, x = xa + (xb - xa) * tt, y = ya + (yb - ya) * ss,
             fill = fill[id])
}

#' Journey plot theme
#'
#' A blank theme with the step axis labels kept, used by the journey plots.
#'
#' @param base_size,base_family Passed to [ggplot2::theme_void()].
#' @return A ggplot2 theme.
#' @export
theme_journey <- function(base_size = 11, base_family = "") {
  ggplot2::theme_void(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(colour = "grey30",
                                          margin = ggplot2::margin(t = 4)),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      plot.subtitle = ggplot2::element_text(colour = "grey30")
    )
}
