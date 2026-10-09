#' Plot a Static Sunburst of Treatment Sequences
#'
#' Draw a treatment-pattern sunburst (ring chart) using `ggplot2` primitives
#' only (`geom_rect` + `coord_polar` + `geom_text`) -- the output is a fully
#' static `ggplot` object suitable for publications and regulatory documents.
#'
#' Each ring is one treatment line (the innermost ring is the first line) and
#' the arc length is the number of patients (one patient = one angular unit,
#' so no normalisation is applied). The child arcs of a segment start at the
#' parent's start angle and nest inside the parent's span; whatever remains of
#' the parent's span stays **empty** -- those are the patients with no further
#' treatment line (attrition), the same idea as the unlinked part of a
#' `sp_plot_sankey()` node.
#'
#' The input is a **path table**: one row per treatment sequence, with one
#' column per ring (`path_cols`) and a count column (`path_value`). `NA` (or
#' `""`) in a ring column means the path stops before that ring; once a path
#' has stopped it must stay `NA` in all later columns. Rows describing the
#' same sequence are summed.
#'
#' @param paths A data frame of treatment sequences (one row per path).
#' @param path_cols Character vector naming the ring columns of `paths`, in
#'   ring order (inner first). Default: every column except `path_value`, in
#'   the order they appear.
#' @param path_value Column name in `paths` holding the patient count of each
#'   path.
#' @param levels Optional character vector fixing the order of the segment
#'   categories within every ring. Default: order of first appearance across
#'   the ring columns.
#' @param segment_labels Optional named character vector mapping a category to
#'   the display text used on the arcs and in the legend (e.g.
#'   `c(IO = "Immunotherapy")`). Unmapped categories show verbatim.
#' @param palette Optional colour palette for the categories. A named vector
#'   is matched by category name; an unnamed vector is recycled over `levels`.
#'   Default: `grDevices::hcl.colors()` "Set 2".
#' @param segment_color Border colour of the arcs.
#' @param ring_height Radial thickness of each ring, in `(0, 1]` ring units.
#' @param inner_radius Radius of the centre hole, in ring units.
#' @param show_labels Whether to draw the arc labels.
#' @param label_size Arc label size.
#' @param label_color Arc label colour.
#' @param label_min_frac Minimum share of the full circle an arc must span for
#'   its label to be drawn (labels on thinner arcs are suppressed).
#' @param center_label Optional text drawn in the centre hole (e.g.
#'   `"All patients\nN = 200"`).
#' @param center_size Centre label size.
#' @param center_color Centre label colour.
#'
#' @return A `ggplot` object.
#' @examples
#' # One shared dataset, two views. This path table is the first three lines
#' # of the sp_plot_sankey() example's 200-patient cohort: one row per treatment
#' # sequence (C = Chemo, IO = Immunotherapy, T = Targeted, N = No
#' # Treatment), `n` patients per path (L1 200 -> L2 120 -> L3 70, about 30%
#' # "No Treatment" per line, no same-treatment transitions). A name like
#' # "C,IO" is a path that stops after the second line.
#' seq_n <- c(
#'   "C,IO,C" = 11, "C,IO,T" = 2, "C,IO,N" = 4, "C,IO" = 3,
#'   "C,T,C" = 4, "C,T,IO" = 3, "C,T,N" = 2, "C,T" = 5,
#'   "C,N" = 24, "C" = 12,
#'   "IO,C,IO" = 10, "IO,C,T" = 7, "IO,C,N" = 9, "IO,C" = 2,
#'   "IO,T,C" = 1, "IO,T,IO" = 1, "IO,T,N" = 1, "IO,T" = 1,
#'   "IO,N" = 8, "IO" = 5,
#'   "T,C,IO" = 3, "T,C,T" = 2, "T,C,N" = 3, "T,C" = 2,
#'   "T,IO,C" = 4, "T,IO,T" = 1, "T,IO,N" = 2, "T,IO" = 1,
#'   "T,N" = 4, "T" = 3,
#'   "N" = 60
#' )
#' parts <- strsplit(names(seq_n), ",")
#' paths <- data.frame(
#'   line1 = vapply(parts, `[`, character(1), 1),
#'   line2 = vapply(parts, `[`, character(1), 2),
#'   line3 = vapply(parts, `[`, character(1), 3),
#'   n = unname(seq_n),
#'   stringsAsFactors = FALSE
#' )
#'
#' trt <- c(C = "Chemo", IO = "Immunotherapy", T = "Targeted",
#'          N = "No Treatment")
#' trt_colors <- c(C = "#2F6C8F", IO = "#4B9F7A", T = "#D08C3E",
#'                 N = "#7F7F7F")
#'
#' # The SAME paths feed a sankey: per-line totals give the node sizes
#' # (including the link-less "L1: No Treatment" node) and consecutive-line
#' # transitions give the links -- exactly the L1-L3 slice of the
#' # sp_plot_sankey() example's node and link tables.
#' ring <- c("line1", "line2", "line3")
#' nodes <- do.call(rbind, lapply(1:3, function(k) {
#'   data.frame(
#'     id        = paste0("L", k, "_", names(trt)),
#'     stage     = paste0("Line", k),
#'     line      = paste0("Line", k),
#'     treatment = unname(trt),
#'     label     = paste0("L", k, ": ", unname(trt)),
#'     node_n    = as.vector(tapply(paths$n, paths[[ring[k]]], sum)[names(trt)]),
#'     stringsAsFactors = FALSE
#'   )
#' }))
#' links <- do.call(rbind, lapply(1:2, function(k) {
#'   move <- !is.na(paths[[ring[k + 1]]])
#'   agg <- aggregate(
#'     list(value = paths$n[move]),
#'     list(s = paths[[ring[k]]][move], t = paths[[ring[k + 1]]][move]),
#'     sum
#'   )
#'   data.frame(
#'     source = paste0("L", k, "_", agg$s),
#'     target = paste0("L", k + 1, "_", agg$t),
#'     value  = agg$value,
#'     stringsAsFactors = FALSE
#'   )
#' }))
#'
#' # Transition view: who moves where between lines.
#' sp_plot_sankey(
#'   nodes = nodes,
#'   links = links,
#'   node_id = "id",
#'   node_stage = "stage",
#'   node_label = "label",
#'   node_value = "node_n",
#'   link_source = "source",
#'   link_target = "target",
#'   link_value = "value",
#'   node_treatment = "treatment",
#'   node_line = "line",
#'   treatment_color_mode = "across_lines",
#'   treatment_palette = stats::setNames(trt_colors, trt),
#'   baseline = "top",
#'   link_alpha = 0.55,
#'   label_size = 2.5
#' )
#'
#' # Path view of the very same data: each ring is a treatment line, arc
#' # length is patients, and the empty remainder of a parent arc is the
#' # attrition (the sunburst counterpart of the sankey's unlinked node span).
#' sp_plot_sunburst(
#'   paths,
#'   path_cols = ring,
#'   path_value = "n",
#'   levels = names(trt),
#'   segment_labels = c(C = "Chemo", IO = "IO", T = "Target", N = "NoTx"),
#'   palette = trt_colors,
#'   center_label = "All patients\nN = 200"
#' )
#' @export
#' @importFrom rlang .data
sp_plot_sunburst <- function(
    paths,
    path_cols = NULL,
    path_value = "n",
    levels = NULL,
    segment_labels = NULL,
    palette = NULL,
    segment_color = "white",
    ring_height = 0.92,
    inner_radius = 1,
    show_labels = TRUE,
    label_size = 2.6,
    label_color = "white",
    label_min_frac = 0.02,
    center_label = NULL,
    center_size = 4.2,
    center_color = "#1F2A30"
) {
  if (!is.data.frame(paths) || nrow(paths) == 0) {
    stop("`paths` must be a non-empty data.frame.")
  }
  if (!path_value %in% names(paths)) {
    stop("`path_value` column not found in `paths`.")
  }
  if (is.null(path_cols)) {
    path_cols <- setdiff(names(paths), path_value)
  }
  if (length(path_cols) == 0) {
    stop("`path_cols` must name at least one ring column.")
  }
  missing_cols <- setdiff(path_cols, names(paths))
  if (length(missing_cols) > 0) {
    stop("Missing path columns: ", paste(missing_cols, collapse = ", "))
  }
  if (!is.numeric(ring_height) || length(ring_height) != 1 || is.na(ring_height) ||
      ring_height <= 0 || ring_height > 1) {
    stop("`ring_height` must be in (0, 1].")
  }
  if (!is.numeric(inner_radius) || length(inner_radius) != 1 || is.na(inner_radius) ||
      inner_radius < 0) {
    stop("`inner_radius` must be >= 0.")
  }
  if (!is.numeric(label_min_frac) || length(label_min_frac) != 1 ||
      is.na(label_min_frac) || label_min_frac < 0 || label_min_frac > 1) {
    stop("`label_min_frac` must be in [0, 1].")
  }

  path_df <- paths
  for (col in path_cols) {
    v <- as.character(path_df[[col]])
    v[!is.na(v) & !nzchar(v)] <- NA_character_
    path_df[[col]] <- v
  }
  path_df$.n <- suppressWarnings(as.numeric(path_df[[path_value]]))
  if (anyNA(path_df$.n) || any(path_df$.n < 0)) {
    stop("`path_value` must be non-negative numbers.")
  }
  path_df <- path_df[path_df$.n > 0, , drop = FALSE]
  if (nrow(path_df) == 0) {
    stop("`paths` has no rows with a positive count.")
  }

  # A path that has stopped must stay NA in every later ring.
  if (length(path_cols) > 1) {
    for (k in seq_len(length(path_cols) - 1)) {
      stopped <- is.na(path_df[[path_cols[k]]])
      resumed <- stopped & !is.na(path_df[[path_cols[k + 1]]])
      if (any(resumed)) {
        stop(
          "`paths` row(s) resume after an NA ring column: ",
          paste(utils::head(which(resumed), 5), collapse = ", ")
        )
      }
    }
  }

  observed <- unique(unlist(lapply(path_cols, function(col) {
    v <- path_df[[col]]
    v[!is.na(v)]
  }), use.names = FALSE))
  if (is.null(levels)) {
    levels <- observed
  } else {
    levels <- as.character(levels)
    unknown <- setdiff(observed, levels)
    if (length(unknown) > 0) {
      stop("Categories missing from `levels`: ", paste(unknown, collapse = ", "))
    }
  }

  if (is.null(palette)) {
    base_palette <- grDevices::hcl.colors(max(length(levels), 3), palette = "Set 2")
  } else {
    base_palette <- as.character(palette)
  }
  if (!is.null(names(palette)) && any(nzchar(names(palette)))) {
    pal_map <- stats::setNames(rep(base_palette, length.out = length(levels)), levels)
    named <- palette[nzchar(names(palette))]
    pal_map[names(named)[names(named) %in% levels]] <-
      as.character(named[names(named) %in% levels])
  } else {
    pal_map <- stats::setNames(rep(base_palette, length.out = length(levels)), levels)
  }

  display <- stats::setNames(levels, levels)
  if (!is.null(segment_labels) && !is.null(names(segment_labels))) {
    hit <- names(segment_labels)[names(segment_labels) %in% levels]
    display[hit] <- as.character(segment_labels[hit])
  }

  total_n <- sum(path_df$.n)

  # One arc per (ring, category, parent path); children start at the parent's
  # start angle, so the remainder of the parent's span stays empty.
  assign_ring <- function(df, ring, x0) {
    out <- vector("list", 0)
    col <- path_cols[ring]
    present <- df[!is.na(df[[col]]), , drop = FALSE]
    for (cat in levels) {
      sub <- present[present[[col]] == cat, , drop = FALSE]
      n <- sum(sub$.n)
      if (n == 0) {
        next
      }
      out[[length(out) + 1]] <- data.frame(
        ring = ring, category = cat, n = n,
        xmin = x0, xmax = x0 + n,
        stringsAsFactors = FALSE
      )
      if (ring < length(path_cols)) {
        child <- assign_ring(sub, ring + 1, x0)
        if (nrow(child) > 0) {
          out[[length(out) + 1]] <- child
        }
      }
      x0 <- x0 + n
    }
    if (length(out) == 0) {
      return(data.frame(
        ring = integer(0), category = character(0), n = numeric(0),
        xmin = numeric(0), xmax = numeric(0), stringsAsFactors = FALSE
      ))
    }
    do.call(rbind, out)
  }
  segs <- assign_ring(path_df, 1, 0)

  segs$ymin <- inner_radius + (segs$ring - 1)
  segs$ymax <- segs$ymin + ring_height

  # Radial labels, flipped on the left half so they always read outward.
  segs$mid <- (segs$xmin + segs$xmax) / 2
  phi <- (90 - 360 * segs$mid / total_n) %% 360
  segs$label_angle <- ifelse(phi > 90 & phi < 270, phi - 180, phi)
  segs$label <- ifelse(
    (segs$xmax - segs$xmin) / total_n >= label_min_frac,
    paste0(unname(display[segs$category]), "\n", segs$n),
    ""
  )

  y_top <- inner_radius + (length(path_cols) - 1) + ring_height

  p <- ggplot2::ggplot(segs) +
    ggplot2::geom_rect(
      mapping = ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax,
        fill = .data$category
      ),
      color = segment_color,
      linewidth = 0.4
    ) +
    ggplot2::scale_fill_manual(
      values = pal_map,
      breaks = levels,
      labels = unname(display[levels]),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(limits = c(0, total_n)) +
    ggplot2::scale_y_continuous(limits = c(0, y_top)) +
    ggplot2::coord_polar(theta = "x", start = 0, direction = 1) +
    ggplot2::theme_void()

  if (show_labels) {
    p <- p + ggplot2::geom_text(
      mapping = ggplot2::aes(
        x = .data$mid, y = (.data$ymin + .data$ymax) / 2,
        label = .data$label, angle = .data$label_angle
      ),
      size = label_size,
      color = label_color,
      fontface = "bold",
      lineheight = 0.9
    )
  }

  if (!is.null(center_label)) {
    p <- p + ggplot2::annotate(
      "text",
      x = 0, y = 0, label = center_label,
      size = center_size, color = center_color,
      fontface = "bold", lineheight = 1.1
    )
  }

  p
}
