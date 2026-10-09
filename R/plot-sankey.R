if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "x", "y", "poly_id", ".link_fill",
    "xmin", "xmax", "ymin", "ymax", ".node_fill",
    "label_x", "label_y", "label"
  ))
}

#' Plot Sankey Diagram with Rectangular Nodes and Bezier Polygon Links
#'
#' Draw a Sankey-style plot using `ggplot2` primitives (`geom_rect` for nodes and
#' `geom_polygon` for links).
#'
#' A key feature of this implementation is that nodes are laid out from the node
#' table rather than inferred from the links, so a node with **no links at all**
#' is still drawn, sized by its `node_value`. For example, a "L1: No Treatment"
#' group that no patients enter or leave still appears in the diagram as a
#' standalone node (see the example).
#'
#' @param nodes A data frame of nodes.
#' @param links A data frame of links.
#' @param node_id Column name in `nodes` for unique node IDs.
#' @param node_stage Column name in `nodes` for stage/layer.
#' @param node_label Optional column name in `nodes` for labels.
#' @param node_value Optional column name in `nodes` for node values. If `NULL`,
#'   values are derived from links.
#' @param link_source Column name in `links` for source node IDs.
#' @param link_target Column name in `links` for target node IDs.
#' @param link_value Column name in `links` for link values.
#' @param orientation Plot direction: `"horizontal"` or `"vertical"`.
#' @param baseline Node stacking baseline: `"top"` or `"bottom"`. Link
#'   ribbons follow it: with `"top"` they stack from the node top downward
#'   (the ribbon toward the top-most counterpart node attaches at the top),
#'   with `"bottom"` from the node bottom upward.
#' @param scale_mode Scale behaviour: `"auto"`, `"shared"`, or `"adaptive"`.
#' @param shared_scale_max Shared maximum span used in `"shared"` mode and as
#'   reference in `"adaptive"` mode.
#' @param adaptive_max_multiplier Maximum magnification in `"adaptive"` mode.
#'   Effective minimum span is `shared_scale_max / adaptive_max_multiplier`.
#' @param node_width Width of each node along stage axis.
#' @param node_gap Gap between nodes in the same stage, in value units.
#'   Default `NULL` resolves it automatically as
#'   `label_gap_scale * label_size * scale basis` (the value span the panel is
#'   scaled against), so the centre-anchored node labels keep at least one
#'   label-sized gap apart no matter how small the nodes are -- plots sharing
#'   a scale (`scale_mode = "shared"`) all get the identical gap. Pass a
#'   number for a fixed gap (the old default was `0.03`). With
#'   `show_labels = FALSE` the automatic gap is a hairline
#'   (`0.002 *` basis).
#' @param node_min_size Minimum visible node size (keeps isolated zero-value
#'   nodes visible).
#' @param link_curvature Bezier curvature factor in `[0, 1]`.
#' @param bezier_n Number of points used to sample Bezier curves.
#' @param node_fill Node fill color, or a column name in `nodes` containing
#'   colors.
#' @param node_treatment Optional column name in `nodes` for treatment class
#'   (e.g., Chemo, IO, Targeted, No Treatment). If supplied, node colors can be
#'   assigned by treatment with `treatment_color_mode`.
#' @param node_line Optional column name in `nodes` for line identifier used
#'   when `treatment_color_mode = "by_line"`. If `NULL`, `node_stage` is used.
#' @param treatment_color_mode Treatment color mapping mode:
#'   `"across_lines"` keeps same treatment color across lines,
#'   `"by_line"` reassigns colors per line.
#' @param treatment_palette Optional color palette (character vector). Named
#'   vectors are matched by treatment names.
#' @param node_color Node border color (default black). Use any color, or
#'   `NA` for no border.
#' @param node_alpha Node alpha.
#' @param link_fill Link fill color, or a column name in `links` containing
#'   colors. The default is a single light grey; use
#'   `use_link_color_by_source` / `use_link_color_by_target` to color ribbons
#'   by their end nodes instead.
#' @param link_color Link border color.
#' @param link_alpha Link alpha.
#' @param show_labels Whether to draw labels.
#' @param label_size Label size.
#' @param label_color Label color.
#' @param label_nudge Label offset from node edge.
#' @param label_gap_scale Proportionality constant for the automatic
#'   `node_gap` (see there); the auto gap is
#'   `label_gap_scale * label_size * scale basis`. Ignored when `node_gap` is
#'   given as a number.
#' @param label_position Side of the node the labels are drawn on, along the
#'   stage axis: `"right"` (default) or `"left"`. For
#'   `orientation = "vertical"` this maps to above (`"right"`) / below
#'   (`"left"`) the nodes. When labels are shown, the panel is extended on the
#'   label side of the terminal stage by the span a link occupies between two
#'   adjacent stages (`1 - node_width`) plus `label_nudge`, so terminal-stage
#'   labels get the same room as the ones drawn between stages instead of
#'   being clipped at the device edge.
#' @param use_link_color_by_source If `TRUE`, link fill uses source node color.
#' @param use_link_color_by_target If `TRUE`, link fill uses target node color.
#'
#' @return A `ggplot` object.
#' @examples
#' # A five-line oncology treatment-sequence cohort (L1 -> L5), 200 patients.
#' #
#' # The node counts behave like a real cohort: at every line about 30% of the
#' # patients are "No Treatment", and the total N of a line never exceeds the
#' # treated (non-"No Treatment") N of the previous line -- L1 treats
#' # 70 + 45 + 25 = 140 patients and L2 holds 120 in total; the other 20 ended
#' # follow-up at L1 without even becoming "L2: No Treatment".
#' #
#' # Highlighted feature: nodes are laid out from the node table, so a node
#' # with NO links at all is still drawn, sized by `node_value`. Here
#' # "L1: No Treatment" is such an isolated node -- no patients flow into or
#' # out of it -- yet it appears in the diagram.
#' trt <- c("Chemo", "Immunotherapy", "Targeted", "No Treatment")
#' key <- c("Chemo", "IO", "Target", "NoTx")
#'
#' nodes <- data.frame(
#'   id        = paste0("L", rep(1:5, each = 4), "_", rep(key, 5)),
#'   stage     = paste0("Line", rep(1:5, each = 4)),
#'   line      = paste0("Line", rep(1:5, each = 4)),
#'   treatment = rep(trt, 5),
#'   label     = paste0("L", rep(1:5, each = 4), ": ", rep(trt, 5)),
#'   node_n    = c(
#'     70, 45, 25, 60, # L1: 140 treated + 60 untreated (30% of 200)
#'     38, 28, 18, 36, # L2: 120 of the 140 L1-treated reach L2 (30% untreated)
#'     20, 17, 12, 21, # L3:  70 of the  84 L2-treated reach L3 (30% untreated)
#'     11, 10,  7, 12, # L4:  40 of the  49 L3-treated reach L4 (30% untreated)
#'      6,  5,  4,  7  # L5:  22 of the  28 L4-treated reach L5 (32% untreated)
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Flows between consecutive lines: a new line implies a CHANGE of therapy,
#' # so there are no same-treatment links, and every treated node also feeds
#' # the next line's "No Treatment" node (patients who progressed but took no
#' # further therapy). One value per source-target pair with the source
#' # cycling fastest. Column sums equal the target node sizes and row sums
#' # never exceed the source node sizes.
#' active <- c("Chemo", "IO", "Target")
#' flows <- list(
#'   c(28, 10, 20, 8, 14, 4, 24, 8, 4), # L1 -> L2
#'   c(15, 5, 13, 4, 9, 3, 12, 6, 3), # L2 -> L3
#'   c(8, 3, 8, 2, 5, 2, 6, 4, 2), # L3 -> L4
#'   c(4, 2, 4, 1, 2, 2, 3, 2, 2) # L4 -> L5
#' )
#' links <- do.call(rbind, lapply(1:4, function(k) {
#'   g <- expand.grid(s = active, t = c(active, "NoTx"), stringsAsFactors = FALSE)
#'   g <- g[g$s != g$t, , drop = FALSE]
#'   data.frame(
#'     source = paste0("L", k, "_", g$s),
#'     target = paste0("L", k + 1, "_", g$t),
#'     value  = flows[[k]],
#'     stringsAsFactors = FALSE
#'   )
#' }))
#'
#' treatment_palette <- c(
#'   "Chemo"         = "#2F6C8F",
#'   "Immunotherapy" = "#4B9F7A",
#'   "Targeted"      = "#D08C3E",
#'   "No Treatment"  = "#7F7F7F"
#' )
#'
#' # Links use the default single light grey; set
#' # `use_link_color_by_source = TRUE` to color the ribbons by their source
#' # node instead.
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
#'   treatment_palette = treatment_palette,
#'   baseline = "top",
#'   link_alpha = 0.55,
#'   label_size = 2.5
#' )
#' @export
#' @importFrom rlang .data
sp_plot_sankey <- function(
    nodes,
    links,
    node_id = "id",
    node_stage = "stage",
    node_label = NULL,
    node_value = NULL,
    link_source = "source",
    link_target = "target",
    link_value = "value",
    orientation = c("horizontal", "vertical"),
    baseline = c("top", "bottom"),
    scale_mode = c("auto", "shared", "adaptive"),
    shared_scale_max = NULL,
    adaptive_max_multiplier = 3,
    node_width = 0.18,
    node_gap = NULL,
    node_min_size = 0.015,
    link_curvature = 0.45,
    bezier_n = 60,
    node_fill = "#4F6D7A",
    node_treatment = NULL,
    node_line = NULL,
    treatment_color_mode = c("across_lines", "by_line"),
    treatment_palette = NULL,
    node_color = "black",
    node_alpha = 1,
    link_fill = "#CCCCCC",
    link_color = NA,
    link_alpha = 0.6,
    show_labels = TRUE,
    label_size = 3,
    label_color = "#1F2A30",
    label_nudge = 0.03,
    label_gap_scale = 0.03,
    label_position = c("right", "left"),
    use_link_color_by_source = FALSE,
    use_link_color_by_target = FALSE
) {
  orientation <- match.arg(orientation)
  baseline <- match.arg(baseline)
  scale_mode <- match.arg(scale_mode)
  treatment_color_mode <- match.arg(treatment_color_mode)
  label_position <- match.arg(label_position)

  if (!is.data.frame(nodes) || nrow(nodes) == 0) {
    stop("`nodes` must be a non-empty data.frame.")
  }
  if (!is.data.frame(links)) {
    stop("`links` must be a data.frame.")
  }

  required_node_cols <- c(node_id, node_stage)
  missing_node_cols <- setdiff(required_node_cols, names(nodes))
  if (length(missing_node_cols) > 0) {
    stop("Missing node columns: ", paste(missing_node_cols, collapse = ", "))
  }

  required_link_cols <- c(link_source, link_target, link_value)
  missing_link_cols <- setdiff(required_link_cols, names(links))
  if (length(missing_link_cols) > 0) {
    stop("Missing link columns: ", paste(missing_link_cols, collapse = ", "))
  }

  if (!is.null(node_label) && !node_label %in% names(nodes)) {
    stop("`node_label` column not found in `nodes`.")
  }

  if (!is.null(node_value) && !node_value %in% names(nodes)) {
    stop("`node_value` column not found in `nodes`.")
  }

  if (!is.null(node_treatment) && !node_treatment %in% names(nodes)) {
    stop("`node_treatment` column not found in `nodes`.")
  }

  if (!is.null(node_line) && !node_line %in% names(nodes)) {
    stop("`node_line` column not found in `nodes`.")
  }

  if (!is.numeric(adaptive_max_multiplier) || adaptive_max_multiplier <= 0) {
    stop("`adaptive_max_multiplier` must be a positive number.")
  }

  if (!is.numeric(node_width) || node_width <= 0) {
    stop("`node_width` must be > 0.")
  }

  if (!is.null(node_gap) && (!is.numeric(node_gap) || length(node_gap) != 1 || is.na(node_gap) || node_gap < 0)) {
    stop("`node_gap` must be NULL (automatic) or a single number >= 0.")
  }

  if (!is.numeric(label_gap_scale) || length(label_gap_scale) != 1 || is.na(label_gap_scale) || label_gap_scale < 0) {
    stop("`label_gap_scale` must be a single number >= 0.")
  }

  if (!is.numeric(bezier_n) || bezier_n < 8) {
    stop("`bezier_n` must be >= 8.")
  }

  bezier_n <- as.integer(bezier_n)
  link_curvature <- max(0, min(1, link_curvature))

  resolve_style <- function(df, style_value, default_value) {
    if (length(style_value) == 1 && is.character(style_value) && style_value %in% names(df)) {
      return(as.character(df[[style_value]]))
    }
    if (length(style_value) == 1) {
      return(rep(as.character(style_value), nrow(df)))
    }
    if (length(style_value) == nrow(df)) {
      return(as.character(style_value))
    }
    rep(as.character(default_value), nrow(df))
  }

  bezier_cubic <- function(p0, p1, p2, p3, n = 60) {
    t <- seq(0, 1, length.out = n)
    (1 - t)^3 * p0 + 3 * (1 - t)^2 * t * p1 + 3 * (1 - t) * t^2 * p2 + t^3 * p3
  }

  node_df <- data.frame(
    node_id = as.character(nodes[[node_id]]),
    stage = as.character(nodes[[node_stage]]),
    stringsAsFactors = FALSE
  )

  node_df$label <- if (is.null(node_label)) node_df$node_id else as.character(nodes[[node_label]])
  node_df$value_input <- if (is.null(node_value)) 0 else suppressWarnings(as.numeric(nodes[[node_value]]))
  node_df$value_input[is.na(node_df$value_input)] <- 0
  node_df$treatment <- if (is.null(node_treatment)) NA_character_ else as.character(nodes[[node_treatment]])
  node_df$line_id <- if (is.null(node_line)) as.character(nodes[[node_stage]]) else as.character(nodes[[node_line]])

  link_df <- data.frame(
    source = as.character(links[[link_source]]),
    target = as.character(links[[link_target]]),
    value = suppressWarnings(as.numeric(links[[link_value]])),
    stringsAsFactors = FALSE
  )
  # Resolve the fill while link_df still has one row per input link, so the
  # colors survive the zero-value / unknown-node filtering below.
  link_df$.link_fill <- resolve_style(links, link_fill, "#CCCCCC")
  link_df$value[is.na(link_df$value)] <- 0
  link_df <- link_df[link_df$value > 0, , drop = FALSE]

  if (nrow(link_df) > 0) {
    valid <- link_df$source %in% node_df$node_id & link_df$target %in% node_df$node_id
    link_df <- link_df[valid, , drop = FALSE]
  }

  in_flow <- stats::setNames(rep(0, nrow(node_df)), node_df$node_id)
  out_flow <- stats::setNames(rep(0, nrow(node_df)), node_df$node_id)
  if (nrow(link_df) > 0) {
    in_tmp <- tapply(link_df$value, link_df$target, sum)
    out_tmp <- tapply(link_df$value, link_df$source, sum)
    in_flow[names(in_tmp)] <- as.numeric(in_tmp)
    out_flow[names(out_tmp)] <- as.numeric(out_tmp)
  }

  node_df$value <- pmax(node_df$value_input, as.numeric(in_flow[node_df$node_id]), as.numeric(out_flow[node_df$node_id]), 0)
  node_df$display_value <- pmax(node_df$value, node_min_size)

  # Automatic node gap: labels are centre-anchored on the nodes, so within a
  # stage adjacent labels sit at least one gap apart. A fixed gap in value
  # units is invisible once the scale spans hundreds of patients, so the
  # automatic gap is proportional to the label text size AND to the value
  # span the panel is scaled against (in shared/adaptive modes at least the
  # shared reference, so every plot sharing a scale gets the identical gap).
  if (is.null(node_gap)) {
    raw_span <- tapply(node_df$display_value, as.character(node_df$stage), sum)
    gap_basis <- max(raw_span)
    if (!is.finite(gap_basis) || gap_basis <= 0) {
      gap_basis <- 1
    }
    shared_ok <- is.numeric(shared_scale_max) && length(shared_scale_max) == 1 &&
      !is.na(shared_scale_max) && shared_scale_max > 0
    if (scale_mode == "shared" && shared_ok) {
      gap_basis <- max(gap_basis, shared_scale_max)
    } else if (scale_mode == "adaptive" && shared_ok) {
      gap_basis <- max(gap_basis, shared_scale_max / adaptive_max_multiplier)
    }
    node_gap <- if (show_labels) {
      label_gap_scale * label_size * gap_basis
    } else {
      0.002 * gap_basis
    }
  }

  if (!all(is.na(node_df$treatment))) {
    rotate_palette <- function(x, k) {
      if (length(x) <= 1) {
        return(x)
      }
      k <- k %% length(x)
      if (k == 0) {
        return(x)
      }
      c(x[(k + 1):length(x)], x[1:k])
    }

    trt_levels <- unique(node_df$treatment)
    trt_levels <- trt_levels[!is.na(trt_levels)]

    if (is.null(treatment_palette)) {
      base_palette <- grDevices::hcl.colors(max(length(trt_levels), 3), palette = "Set 2")
    } else {
      base_palette <- as.character(treatment_palette)
    }

    if (length(base_palette) == 0) {
      base_palette <- c("#4F6D7A")
    }

    if (!is.null(names(treatment_palette)) && any(nzchar(names(treatment_palette)))) {
      named_pal <- treatment_palette
      fallback <- rep(base_palette, length.out = length(trt_levels))
      names(fallback) <- trt_levels
      fallback[names(named_pal)] <- as.character(named_pal)
      trt_map <- fallback
    } else {
      trt_map <- rep(base_palette, length.out = length(trt_levels))
      names(trt_map) <- trt_levels
    }

    if (treatment_color_mode == "across_lines") {
      node_df$.node_fill <- unname(trt_map[node_df$treatment])
    } else {
      line_levels <- unique(node_df$line_id)
      line_levels <- line_levels[!is.na(line_levels)]
      node_df$.node_fill <- NA_character_

      for (li in seq_along(line_levels)) {
        lv <- line_levels[li]
        idx <- which(node_df$line_id == lv)
        if (length(idx) == 0) {
          next
        }
        trt_in_line <- unique(node_df$treatment[idx])
        trt_in_line <- trt_in_line[!is.na(trt_in_line)]

        pal_line <- rotate_palette(base_palette, li - 1)
        if (!is.null(names(treatment_palette)) && any(nzchar(names(treatment_palette)))) {
          pal_map_line <- rep(pal_line, length.out = length(trt_in_line))
          names(pal_map_line) <- trt_in_line
          pal_map_line[names(treatment_palette)] <- as.character(treatment_palette)
        } else {
          pal_map_line <- rep(pal_line, length.out = length(trt_in_line))
          names(pal_map_line) <- trt_in_line
        }

        node_df$.node_fill[idx] <- unname(pal_map_line[node_df$treatment[idx]])
      }
    }

    node_df$.node_fill[is.na(node_df$.node_fill)] <- "#4F6D7A"
  } else {
    node_df$.node_fill <- resolve_style(nodes, node_fill, "#4F6D7A")
  }

  stage_levels <- unique(node_df$stage)
  node_df$stage <- factor(node_df$stage, levels = stage_levels)
  node_df$node_index <- seq_len(nrow(node_df))
  node_df <- node_df[order(node_df$stage, node_df$node_index), , drop = FALSE]

  n_stage <- length(stage_levels)
  stage_position <- stats::setNames(seq_len(n_stage) - 1, stage_levels)
  node_df$p_center <- as.numeric(stage_position[as.character(node_df$stage)])
  node_df$pmin <- node_df$p_center - node_width / 2
  node_df$pmax <- node_df$p_center + node_width / 2

  stage_ids <- as.character(node_df$stage)
  stage_span <- tapply(node_df$display_value, stage_ids, sum)
  stage_count <- table(stage_ids)
  stage_span <- stage_span + pmax(0, as.numeric(stage_count) - 1) * node_gap

  local_scale_max <- max(stage_span)
  if (!is.finite(local_scale_max) || local_scale_max <= 0) {
    local_scale_max <- 1
  }

  if (scale_mode == "auto") {
    scale_max <- local_scale_max
  } else if (scale_mode == "shared") {
    if (is.null(shared_scale_max) || !is.numeric(shared_scale_max) || shared_scale_max <= 0) {
      stop("`shared_scale_max` must be a positive number when `scale_mode = 'shared'`.")
    }
    scale_max <- shared_scale_max
  } else {
    if (is.null(shared_scale_max) || !is.numeric(shared_scale_max) || shared_scale_max <= 0) {
      scale_max <- local_scale_max
    } else {
      min_span <- shared_scale_max / adaptive_max_multiplier
      scale_max <- max(local_scale_max, min_span)
    }
  }

  max_nodes_per_stage <- max(as.numeric(stage_count))
  panel_span <- scale_max + pmax(0, max_nodes_per_stage - 1) * node_gap

  node_df$smin <- 0
  node_df$smax <- 0
  for (st in stage_levels) {
    idx <- which(as.character(node_df$stage) == st)
    if (length(idx) == 0) {
      next
    }
    dvals <- node_df$display_value[idx]
    st_span <- sum(dvals) + pmax(0, length(idx) - 1) * node_gap
    if (baseline == "top") {
      # Keep input order as top -> bottom when top-aligned.
      if (length(idx) == 1) {
        ymax <- panel_span
      } else {
        ymax <- panel_span - c(0, cumsum(dvals[-length(dvals)] + node_gap))
      }
      node_df$smax[idx] <- ymax
      node_df$smin[idx] <- ymax - dvals
    } else {
      # Keep input order as bottom -> top when bottom-aligned.
      base_offset <- 0
      if (length(idx) == 1) {
        starts <- base_offset
      } else {
        starts <- base_offset + c(0, cumsum(dvals[-length(dvals)] + node_gap))
      }
      node_df$smin[idx] <- starts
      node_df$smax[idx] <- starts + dvals
    }
  }

  links_for_plot <- link_df
  polygons <- data.frame(x = numeric(0), y = numeric(0), poly_id = character(0), .link_fill = character(0), stringsAsFactors = FALSE)

  if (nrow(links_for_plot) > 0) {
    links_for_plot$row_id <- seq_len(nrow(links_for_plot))

    # Within a node, ribbons are laid out in the stacking order of the
    # counterpart node (its position in `node_df`, i.e. input order per
    # stage), and the cumulative offsets are anchored on the `baseline` side:
    # top-aligned nodes stack their ribbons from the node top downward,
    # bottom-aligned nodes from the node bottom upward.
    node_pos <- stats::setNames(seq_len(nrow(node_df)), node_df$node_id)

    src_sorted <- links_for_plot[order(links_for_plot$source, node_pos[links_for_plot$target], links_for_plot$row_id), , drop = FALSE]
    src_sorted$source_offset <- stats::ave(src_sorted$value, src_sorted$source, FUN = function(v) c(0, cumsum(utils::head(v, -1))))

    tgt_sorted <- links_for_plot[order(links_for_plot$target, node_pos[links_for_plot$source], links_for_plot$row_id), , drop = FALSE]
    tgt_sorted$target_offset <- stats::ave(tgt_sorted$value, tgt_sorted$target, FUN = function(v) c(0, cumsum(utils::head(v, -1))))

    links_for_plot <- merge(src_sorted, tgt_sorted[, c("row_id", "target_offset")], by = "row_id", all.x = TRUE, sort = FALSE)
    links_for_plot <- links_for_plot[order(links_for_plot$row_id), , drop = FALSE]

    src_idx <- match(links_for_plot$source, node_df$node_id)
    tgt_idx <- match(links_for_plot$target, node_df$node_id)

    links_for_plot$p0 <- node_df$pmax[src_idx]
    links_for_plot$p1 <- node_df$pmin[tgt_idx]
    if (baseline == "top") {
      links_for_plot$s0_max <- node_df$smax[src_idx] - links_for_plot$source_offset
      links_for_plot$s0_min <- links_for_plot$s0_max - links_for_plot$value
      links_for_plot$s1_max <- node_df$smax[tgt_idx] - links_for_plot$target_offset
      links_for_plot$s1_min <- links_for_plot$s1_max - links_for_plot$value
    } else {
      links_for_plot$s0_min <- node_df$smin[src_idx] + links_for_plot$source_offset
      links_for_plot$s0_max <- links_for_plot$s0_min + links_for_plot$value
      links_for_plot$s1_min <- node_df$smin[tgt_idx] + links_for_plot$target_offset
      links_for_plot$s1_max <- links_for_plot$s1_min + links_for_plot$value
    }

    if (use_link_color_by_source) {
      src_fill_map <- stats::setNames(node_df$.node_fill, node_df$node_id)
      links_for_plot$.link_fill <- unname(src_fill_map[links_for_plot$source])
    }

    if (use_link_color_by_target) {
      tgt_fill_map <- stats::setNames(node_df$.node_fill, node_df$node_id)
      links_for_plot$.link_fill <- unname(tgt_fill_map[links_for_plot$target])
    }

    poly_list <- vector("list", nrow(links_for_plot))
    for (i in seq_len(nrow(links_for_plot))) {
      p0 <- links_for_plot$p0[i]
      p1 <- links_for_plot$p1[i]
      dp <- p1 - p0
      c1 <- p0 + link_curvature * dp
      c2 <- p1 - link_curvature * dp

      top_p <- bezier_cubic(p0, c1, c2, p1, n = bezier_n)
      top_s <- bezier_cubic(links_for_plot$s0_max[i], links_for_plot$s0_max[i], links_for_plot$s1_max[i], links_for_plot$s1_max[i], n = bezier_n)
      bot_p <- bezier_cubic(p0, c1, c2, p1, n = bezier_n)
      bot_s <- bezier_cubic(links_for_plot$s0_min[i], links_for_plot$s0_min[i], links_for_plot$s1_min[i], links_for_plot$s1_min[i], n = bezier_n)

      p_vals <- c(top_p, rev(bot_p))
      s_vals <- c(top_s, rev(bot_s))

      if (orientation == "horizontal") {
        x_vals <- p_vals
        y_vals <- s_vals
      } else {
        x_vals <- s_vals
        y_vals <- p_vals
      }

      poly_list[[i]] <- data.frame(
        x = x_vals,
        y = y_vals,
        poly_id = paste0("link_", links_for_plot$row_id[i]),
        .link_fill = links_for_plot$.link_fill[i],
        stringsAsFactors = FALSE
      )
    }
    polygons <- do.call(rbind, poly_list)
  }

  # Labels sit `label_nudge` off the node on the `label_position` side of the
  # stage axis. `geom_text()` does not contribute to the panel range, so the
  # panel is extended on the label side of the terminal stage by the span a
  # link occupies between two adjacent stages (stage centers are 1 apart) plus
  # the nudge; otherwise terminal-stage labels are clipped at the device edge.
  if (label_position == "right") {
    label_p <- node_df$pmax + label_nudge
    label_just <- 0
  } else {
    label_p <- node_df$pmin - label_nudge
    label_just <- 1
  }

  p_lower <- min(node_df$pmin)
  p_upper <- max(node_df$pmax)
  if (show_labels) {
    label_span <- (1 - node_width) + label_nudge
    if (label_position == "right") {
      p_upper <- p_upper + label_span
    } else {
      p_lower <- p_lower - label_span
    }
  }

  if (orientation == "horizontal") {
    node_rect <- data.frame(
      xmin = node_df$pmin,
      xmax = node_df$pmax,
      ymin = node_df$smin,
      ymax = node_df$smax,
      label_x = label_p,
      label_y = (node_df$smin + node_df$smax) / 2,
      hjust = label_just,
      vjust = 0.5,
      label = node_df$label,
      .node_fill = node_df$.node_fill,
      stringsAsFactors = FALSE
    )
  } else {
    node_rect <- data.frame(
      xmin = node_df$smin,
      xmax = node_df$smax,
      ymin = node_df$pmin,
      ymax = node_df$pmax,
      label_x = (node_df$smin + node_df$smax) / 2,
      label_y = label_p,
      hjust = 0.5,
      vjust = label_just,
      label = node_df$label,
      .node_fill = node_df$.node_fill,
      stringsAsFactors = FALSE
    )
  }

  p <- ggplot2::ggplot()

  if (nrow(polygons) > 0) {
    p <- p + ggplot2::geom_polygon(
      data = polygons,
      mapping = ggplot2::aes(x = .data$x, y = .data$y, group = .data$poly_id, fill = .data$.link_fill),
      color = link_color,
      alpha = link_alpha,
      inherit.aes = FALSE
    )
  }

  p <- p +
    ggplot2::geom_rect(
      data = node_rect,
      mapping = ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax, fill = .data$.node_fill),
      color = node_color,
      alpha = node_alpha,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_identity() +
    {
      if (orientation == "horizontal") {
        ggplot2::coord_cartesian(xlim = c(p_lower, p_upper), ylim = c(0, panel_span), clip = "off")
      } else {
        ggplot2::coord_cartesian(xlim = c(0, panel_span), ylim = c(p_lower, p_upper), clip = "off")
      }
    } +
    ggplot2::theme_void()

  if (show_labels) {
    p <- p + ggplot2::geom_text(
      data = node_rect,
      mapping = ggplot2::aes(x = .data$label_x, y = .data$label_y, label = .data$label),
      size = label_size,
      color = label_color,
      hjust = node_rect$hjust,
      vjust = node_rect$vjust,
      inherit.aes = FALSE
    )
  }

  p
}
