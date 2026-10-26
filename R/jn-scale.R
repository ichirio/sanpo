#' Journey colour scale
#'
#' The fill scale used by every journey plot: one fixed colour per state from
#' the states dictionary, all levels kept (`drop = FALSE`) so that a state
#' has the same colour and legend position in the Sankey, the sunburst and
#' the icicle, even when it is empty in one of them. `scale_colour_journey()`
#' is the same for the colour aesthetic (used for the state keys of
#' [plot_transition()]).
#'
#' @param states A [journey()], a [jn_states()] dictionary or a character
#'   vector of levels.
#' @param name Legend title.
#' @param ... Passed to [ggplot2::scale_fill_manual()] or
#'   [ggplot2::scale_colour_manual()].
#' @return A ggplot2 scale.
#' @examples
#' j <- jn_sequence(jn_simulate(100))
#' scale_fill_journey(j)
#' @export
scale_fill_journey <- function(states, name = "State", ...) {
  st <- as_jn_states(states)
  ggplot2::scale_fill_manual(
    name = name,
    values = stats::setNames(st$colour, st$level),
    limits = st$level,
    breaks = st$level,
    labels = st$label,
    drop = FALSE,
    ...
  )
}

#' @rdname scale_fill_journey
#' @export
scale_colour_journey <- function(states, name = "State", ...) {
  st <- as_jn_states(states)
  ggplot2::scale_colour_manual(
    name = name,
    values = stats::setNames(st$colour, st$level),
    limits = st$level,
    breaks = st$level,
    labels = st$label,
    drop = FALSE,
    ...
  )
}

# "n", "pct", "n_pct" or "none" -> label text.
format_count_label <- function(n, pct, label) {
  switch(label,
    n = format(n, big.mark = ","),
    pct = sprintf("%.0f%%", 100 * pct),
    n_pct = sprintf("%s (%.0f%%)", format(n, big.mark = ",", trim = TRUE), 100 * pct),
    none = rep("", length(n))
  )
}

# An invisible zero-size rectangle per level, so that the legend draws a
# coloured key for every state, including states no patient is in.
# `x`, `y`: where to put it, inside the data range so that the axes do not
# grow.
legend_keys_layer <- function(st, x = 0, y = 0) {
  d <- data.frame(state = factor(st$level, levels = st$level), x = x, y = y)
  ggplot2::geom_rect(
    data = d,
    mapping = ggplot2::aes(xmin = .data$x, xmax = .data$x, ymin = .data$y,
                           ymax = .data$y, fill = .data$state),
    colour = NA, inherit.aes = FALSE
  )
}
