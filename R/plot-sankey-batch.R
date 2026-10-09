#' Batch Create Sankey Plots for Subgroup Analysis
#'
#' Create multiple Sankey plots from subgroup-specific filters in one call.
#' The function supports scaling strategies across subgroups:
#'
#' 1) Shared scale across all subgroup plots.
#' 2) First-stage normalization with optional magnification cap, then shared scale.
#' 3) Shared scale anchored to the first-stage span of a reference subgroup.
#'
#' @param nodes Node data frame.
#' @param links Link data frame.
#' @param subgroup_specs Data frame with subgroup definitions.
#'   Recommended columns:
#'   - `subgroup`: subgroup name.
#'   - `filter`: expression string applied to both `nodes` and `links`.
#'   - `node_filter`: expression string for `nodes` only (optional).
#'   - `link_filter`: expression string for `links` only (optional).
#'   - `title`: plot title override (optional).
#'   - `file_name`: output file name (optional).
#' @param node_id,node_stage,node_label,node_value Node mapping columns passed to `sp_plot_sankey()`.
#' @param link_source,link_target,link_value Link mapping columns passed to `sp_plot_sankey()`.
#' @param scale_strategy Scale strategy across subgroups:
#'   `"shared_max"`, `"shared_first_stage"`, or `"first_stage_normalized"`.
#' @param scale_reference_subgroup Reference subgroup name used when
#'   `scale_strategy = "shared_first_stage"`. If `NULL`, the first subgroup in
#'   `subgroup_specs` is used.
#' @param first_stage_max_multiplier Magnification cap used when
#'   `scale_strategy = "first_stage_normalized"` (default 100). Use `Inf` (or
#'   `NULL`) for no cap.
#' @param output_dir Optional output directory to save PNG files.
#' @param save_png Whether to write PNG files.
#' @param width,height,dpi,bg PNG rendering options for `ggsave()`.
#' @param ... Additional arguments passed to `sp_plot_sankey()`.
#'
#' @return A list with:
#'   - `plots`: named list of ggplot objects.
#'   - `metadata`: data frame with subgroup, scales, multipliers, and output file names.
#' @examples
#' # The full analysis set is the same five-line, 200-patient cohort as the
#' # sp_plot_sankey() example; three subgroups of decreasing size (Age < 65,
#' # Age >= 65, Biomarker positive) share its structure. Every cohort
#' # follows the usual rules: about 30% of each line is "No Treatment", a
#' # line's total N never exceeds the treated N of the previous line, there
#' # are no same-treatment transitions, and "L1: No Treatment" is an
#' # isolated node.
#' trt <- c("Chemo", "Immunotherapy", "Targeted", "No Treatment")
#' key <- c("Chemo", "IO", "Target", "NoTx")
#' active <- c("Chemo", "IO", "Target")
#'
#' mk_nodes <- function(grp, n) {
#'   data.frame(
#'     grp       = grp,
#'     id        = paste0("L", rep(1:5, each = 4), "_", rep(key, 5)),
#'     stage     = paste0("Line", rep(1:5, each = 4)),
#'     line      = paste0("Line", rep(1:5, each = 4)),
#'     treatment = rep(trt, 5),
#'     label     = paste0("L", rep(1:5, each = 4), ": ", rep(trt, 5)),
#'     node_n    = n,
#'     stringsAsFactors = FALSE
#'   )
#' }
#' mk_links <- function(grp, flows) {
#'   g <- expand.grid(s = active, t = c(active, "NoTx"), stringsAsFactors = FALSE)
#'   g <- g[g$s != g$t, , drop = FALSE]
#'   do.call(rbind, lapply(1:4, function(k) {
#'     data.frame(
#'       grp    = grp,
#'       source = paste0("L", k, "_", g$s),
#'       target = paste0("L", k + 1, "_", g$t),
#'       value  = flows[[k]],
#'       stringsAsFactors = FALSE
#'     )
#'   }))
#' }
#'
#' nodes <- rbind(
#'   mk_nodes("ALL", c(
#'     70, 45, 25, 60, 38, 28, 18, 36, 20, 17, 12, 21,
#'     11, 10, 7, 12, 6, 5, 4, 7
#'   )),
#'   mk_nodes("AGE_LT65", c(
#'     42, 27, 15, 36, 23, 17, 10, 21, 12, 10, 7, 13,
#'     7, 6, 4, 7, 3, 3, 2, 4
#'   )),
#'   mk_nodes("AGE_GE65", c(
#'     28, 18, 10, 24, 15, 11, 8, 15, 8, 7, 5, 8,
#'     4, 4, 3, 5, 3, 2, 2, 3
#'   )),
#'   mk_nodes("BM_POS", c(
#'     7, 4, 3, 6, 3, 3, 2, 4, 2, 2, 1, 2,
#'     1, 1, 1, 1, 1, 1, 0, 1
#'   ))
#' )
#' links <- rbind(
#'   mk_links("ALL", list(
#'     c(28, 10, 20, 8, 14, 4, 24, 8, 4),
#'     c(15, 5, 13, 4, 9, 3, 12, 6, 3),
#'     c(8, 3, 8, 2, 5, 2, 6, 4, 2),
#'     c(4, 2, 4, 1, 2, 2, 3, 2, 2)
#'   )),
#'   mk_links("AGE_LT65", list(
#'     c(17, 6, 12, 5, 8, 2, 14, 5, 2),
#'     c(9, 3, 8, 2, 5, 2, 7, 4, 2),
#'     c(5, 2, 5, 1, 3, 1, 4, 2, 1),
#'     c(2, 1, 2, 1, 1, 1, 2, 1, 1)
#'   )),
#'   mk_links("AGE_GE65", list(
#'     c(11, 4, 8, 3, 6, 2, 10, 3, 2),
#'     c(6, 2, 5, 2, 4, 1, 5, 2, 1),
#'     c(3, 1, 3, 1, 2, 1, 2, 2, 1),
#'     c(2, 1, 1, 1, 1, 1, 1, 1, 1)
#'   )),
#'   mk_links("BM_POS", list(
#'     c(2, 1, 2, 1, 1, 1, 2, 1, 1),
#'     c(2, 0, 1, 1, 1, 0, 1, 1, 0),
#'     c(1, 0, 1, 0, 1, 0, 0, 1, 0),
#'     c(1, 0, 1, 0, 0, 0, 0, 0, 1)
#'   ))
#' )
#'
#' # Each panel's title states the subgroup condition.
#' specs <- data.frame(
#'   subgroup = c("all", "age_lt65", "age_ge65", "biomarker"),
#'   filter   = c(
#'     'grp == "ALL"', 'grp == "AGE_LT65"',
#'     'grp == "AGE_GE65"', 'grp == "BM_POS"'
#'   ),
#'   title    = c(
#'     "All patients (N = 200)", "Age < 65 (N = 120)",
#'     "Age >= 65 (N = 80)", "Biomarker positive (N = 20)"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' treatment_palette <- c(
#'   "Chemo"         = "#2F6C8F",
#'   "Immunotherapy" = "#4B9F7A",
#'   "Targeted"      = "#D08C3E",
#'   "No Treatment"  = "#7F7F7F"
#' )
#'
#' # 1) Shared scale: every plot uses the same axis span, so node heights
#' #    are directly comparable across subgroups -- the smaller subgroups
#' #    are drawn smaller.
#' shared <- sp_plot_sankey_batch(
#'   nodes, links, specs,
#'   node_label = "label", node_value = "node_n",
#'   scale_strategy = "shared_max",
#'   save_png = FALSE,
#'   node_treatment = "treatment", node_line = "line",
#'   treatment_palette = treatment_palette,
#'   baseline = "top", link_alpha = 0.55, label_size = 1.8
#' )
#'
#' # 2) First-stage normalization: each subgroup is magnified so its Line 1
#' #    column spans the same height -- the shapes (proportions) are
#' #    comparable, the absolute sizes are not. The magnification is capped
#' #    by `first_stage_max_multiplier` (default 100); here the subgroups
#' #    are scaled by about x1.7 / x2.5 / x10.
#' norm <- sp_plot_sankey_batch(
#'   nodes, links, specs,
#'   node_label = "label", node_value = "node_n",
#'   scale_strategy = "first_stage_normalized",
#'   save_png = FALSE,
#'   node_treatment = "treatment", node_line = "line",
#'   treatment_palette = treatment_palette,
#'   baseline = "top", link_alpha = 0.55, label_size = 1.8
#' )
#' norm$metadata[, c("subgroup", "multiplier")]
#'
#' # All patients + the three subgroups on one page (2 x 2), one page per
#' # scale strategy.
#' if (requireNamespace("patchwork", quietly = TRUE)) {
#'   patchwork::wrap_plots(shared$plots, ncol = 2)
#' }
#' if (requireNamespace("patchwork", quietly = TRUE)) {
#'   patchwork::wrap_plots(norm$plots, ncol = 2)
#' }
#' @export
sp_plot_sankey_batch <- function(
    nodes,
    links,
    subgroup_specs,
    node_id = "id",
    node_stage = "stage",
    node_label = NULL,
    node_value = NULL,
    link_source = "source",
    link_target = "target",
    link_value = "value",
    scale_strategy = c("shared_max", "shared_first_stage", "first_stage_normalized"),
    scale_reference_subgroup = NULL,
    first_stage_max_multiplier = 100,
    output_dir = NULL,
    save_png = TRUE,
    width = 14,
    height = 8,
    dpi = 180,
    bg = "white",
    ...
) {
  scale_strategy <- match.arg(scale_strategy)

  if (!is.data.frame(nodes) || nrow(nodes) == 0) {
    stop("`nodes` must be a non-empty data.frame.")
  }
  if (!is.data.frame(links)) {
    stop("`links` must be a data.frame.")
  }
  if (!is.data.frame(subgroup_specs) || nrow(subgroup_specs) == 0) {
    stop("`subgroup_specs` must be a non-empty data.frame.")
  }

  if (!"subgroup" %in% names(subgroup_specs)) {
    subgroup_specs$subgroup <- paste0("subgroup_", seq_len(nrow(subgroup_specs)))
  }

  if (!is.null(scale_reference_subgroup) && (!is.character(scale_reference_subgroup) || length(scale_reference_subgroup) != 1)) {
    stop("`scale_reference_subgroup` must be NULL or a single character string.")
  }

  cap <- first_stage_max_multiplier
  if (is.null(cap)) {
    cap <- Inf
  }
  if (!is.numeric(cap) || length(cap) != 1 || is.na(cap) || cap <= 0) {
    stop("`first_stage_max_multiplier` must be a positive number, `Inf`, or `NULL`.")
  }

  apply_filter_expr <- function(df, expr_text) {
    if (is.null(expr_text) || length(expr_text) == 0 || is.na(expr_text) || !nzchar(expr_text)) {
      return(df)
    }

    idx <- eval(parse(text = expr_text), envir = df, enclos = parent.frame())

    if (!is.logical(idx)) {
      stop("Filter expression must return logical values: ", expr_text)
    }
    if (length(idx) == 1) {
      idx <- rep(idx, nrow(df))
    }
    if (length(idx) != nrow(df)) {
      stop("Filter expression length mismatch: ", expr_text)
    }

    df[which(!is.na(idx) & idx), , drop = FALSE]
  }

  compute_scale_info <- function(nodes_df, links_df, node_gap_in = 0.03, node_min_size_in = 0.015) {
    if (!node_id %in% names(nodes_df) || !node_stage %in% names(nodes_df)) {
      stop("Subgroup nodes are missing required columns.")
    }
    if (!link_source %in% names(links_df) || !link_target %in% names(links_df) || !link_value %in% names(links_df)) {
      stop("Subgroup links are missing required columns.")
    }

    node_tmp <- data.frame(
      node_id = as.character(nodes_df[[node_id]]),
      stage = as.character(nodes_df[[node_stage]]),
      stringsAsFactors = FALSE
    )

    node_tmp$value_input <- if (is.null(node_value)) {
      rep(0, nrow(node_tmp))
    } else {
      suppressWarnings(as.numeric(nodes_df[[node_value]]))
    }
    node_tmp$value_input[is.na(node_tmp$value_input)] <- 0

    link_tmp <- data.frame(
      source = as.character(links_df[[link_source]]),
      target = as.character(links_df[[link_target]]),
      value = suppressWarnings(as.numeric(links_df[[link_value]])),
      stringsAsFactors = FALSE
    )
    link_tmp$value[is.na(link_tmp$value)] <- 0
    link_tmp <- link_tmp[link_tmp$value > 0, , drop = FALSE]

    if (nrow(link_tmp) > 0) {
      valid <- link_tmp$source %in% node_tmp$node_id & link_tmp$target %in% node_tmp$node_id
      link_tmp <- link_tmp[valid, , drop = FALSE]
    }

    in_flow <- stats::setNames(rep(0, nrow(node_tmp)), node_tmp$node_id)
    out_flow <- stats::setNames(rep(0, nrow(node_tmp)), node_tmp$node_id)

    if (nrow(link_tmp) > 0) {
      in_tmp <- tapply(link_tmp$value, link_tmp$target, sum)
      out_tmp <- tapply(link_tmp$value, link_tmp$source, sum)
      in_flow[names(in_tmp)] <- as.numeric(in_tmp)
      out_flow[names(out_tmp)] <- as.numeric(out_tmp)
    }

    value_raw <- pmax(
      node_tmp$value_input,
      as.numeric(in_flow[node_tmp$node_id]),
      as.numeric(out_flow[node_tmp$node_id]),
      0
    )
    value_display <- pmax(value_raw, node_min_size_in)

    stage_levels <- unique(node_tmp$stage)
    stage_ids <- as.character(node_tmp$stage)
    stage_span <- tapply(value_display, stage_ids, sum)
    stage_count <- table(stage_ids)
    stage_span <- stage_span + pmax(0, as.numeric(stage_count) - 1) * node_gap_in

    local_scale_max <- max(stage_span)
    if (!is.finite(local_scale_max) || local_scale_max <= 0) {
      local_scale_max <- 1
    }

    first_stage <- stage_levels[1]
    first_stage_span <- as.numeric(stage_span[first_stage])
    if (!is.finite(first_stage_span) || length(first_stage_span) == 0 || is.na(first_stage_span) || first_stage_span <= 0) {
      first_stage_span <- node_min_size_in
    }

    list(local_scale_max = local_scale_max, first_stage_span = first_stage_span)
  }

  args_extra <- list(...)
  node_gap_in <- if (!is.null(args_extra$node_gap)) args_extra$node_gap else 0.03
  node_min_size_in <- if (!is.null(args_extra$node_min_size)) args_extra$node_min_size else 0.015

  get_col_or_default <- function(df, col_name, default = NA_character_) {
    if (col_name %in% names(df)) as.character(df[[col_name]]) else rep(default, nrow(df))
  }

  filter_all <- get_col_or_default(subgroup_specs, "filter", "")
  filter_nodes <- get_col_or_default(subgroup_specs, "node_filter", "")
  filter_links <- get_col_or_default(subgroup_specs, "link_filter", "")
  title_vec <- get_col_or_default(subgroup_specs, "title", "")
  file_vec <- get_col_or_default(subgroup_specs, "file_name", "")

  subgroup_data <- vector("list", nrow(subgroup_specs))

  for (i in seq_len(nrow(subgroup_specs))) {
    nf <- filter_nodes[i]
    lf <- filter_links[i]

    if (!nzchar(nf)) nf <- filter_all[i]
    if (!nzchar(lf)) lf <- filter_all[i]

    nd <- apply_filter_expr(nodes, nf)
    lk <- apply_filter_expr(links, lf)

    subgroup_data[[i]] <- list(
      subgroup = as.character(subgroup_specs$subgroup[i]),
      title = title_vec[i],
      file_name = file_vec[i],
      nodes = nd,
      links = lk,
      multiplier = 1
    )
  }

  if (scale_strategy == "first_stage_normalized") {
    first_span <- vapply(
      subgroup_data,
      function(x) compute_scale_info(x$nodes, x$links, node_gap_in, node_min_size_in)$first_stage_span,
      numeric(1)
    )

    ref_first <- max(first_span)

    for (i in seq_along(subgroup_data)) {
      mul <- ref_first / first_span[i]
      if (is.finite(cap)) {
        mul <- min(mul, cap)
      }

      subgroup_data[[i]]$multiplier <- mul
      subgroup_data[[i]]$links[[link_value]] <- suppressWarnings(as.numeric(subgroup_data[[i]]$links[[link_value]])) * mul

      if (!is.null(node_value) && node_value %in% names(subgroup_data[[i]]$nodes)) {
        nv <- suppressWarnings(as.numeric(subgroup_data[[i]]$nodes[[node_value]]))
        idx <- !is.na(nv)
        nv[idx] <- nv[idx] * mul
        subgroup_data[[i]]$nodes[[node_value]] <- nv
      }
    }
  }

  scale_info <- lapply(
    subgroup_data,
    function(x) compute_scale_info(x$nodes, x$links, node_gap_in, node_min_size_in)
  )
  local_max <- vapply(scale_info, function(x) x$local_scale_max, numeric(1))
  first_span <- vapply(scale_info, function(x) x$first_stage_span, numeric(1))

  if (scale_strategy == "shared_max") {
    shared_max <- max(local_max)
  } else if (scale_strategy == "shared_first_stage") {
    ref_idx <- 1L
    if (!is.null(scale_reference_subgroup)) {
      idx <- which(vapply(subgroup_data, function(x) identical(x$subgroup, scale_reference_subgroup), logical(1)))[1]
      if (!is.na(idx)) {
        ref_idx <- idx
      } else {
        warning("`scale_reference_subgroup` not found in `subgroup_specs`; using first subgroup as reference.")
      }
    }
    shared_max <- first_span[ref_idx]
  } else {
    shared_max <- max(local_max)
  }

  if (!is.finite(shared_max) || shared_max <= 0) {
    shared_max <- 1
  }

  if (!is.null(output_dir) && save_png && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  plot_list <- list()
  meta <- data.frame(
    subgroup = character(0),
    title = character(0),
    file_name = character(0),
    local_scale_max = numeric(0),
    shared_scale_max = numeric(0),
    multiplier = numeric(0),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(subgroup_data)) {
    s <- subgroup_data[[i]]

    p_args <- c(list(
      nodes = s$nodes,
      links = s$links,
      node_id = node_id,
      node_stage = node_stage,
      node_label = node_label,
      node_value = node_value,
      link_source = link_source,
      link_target = link_target,
      link_value = link_value,
      scale_mode = "shared",
      shared_scale_max = shared_max
    ), args_extra)

    p <- do.call(sp_plot_sankey, p_args)

    title_i <- s$title
    if (!is.na(title_i) && nzchar(title_i)) {
      p <- p + ggplot2::labs(title = title_i)
    }

    subgroup_name <- s$subgroup
    if (is.na(subgroup_name) || !nzchar(subgroup_name)) {
      subgroup_name <- paste0("subgroup_", i)
    }

    file_i <- s$file_name
    if (is.na(file_i) || !nzchar(file_i)) {
      file_i <- paste0("sankey_", subgroup_name, ".png")
    }

    if (save_png) {
      out_path <- if (is.null(output_dir)) file_i else file.path(output_dir, file_i)
      ggplot2::ggsave(
        filename = out_path,
        plot = p,
        width = width,
        height = height,
        dpi = dpi,
        bg = bg
      )
    }

    plot_list[[subgroup_name]] <- p

    meta <- rbind(meta, data.frame(
      subgroup = subgroup_name,
      title = ifelse(is.na(title_i), "", title_i),
      file_name = file_i,
      local_scale_max = local_max[i],
      first_stage_span = first_span[i],
      shared_scale_max = shared_max,
      multiplier = s$multiplier,
      stringsAsFactors = FALSE
    ))
  }

  list(plots = plot_list, metadata = meta)
}
