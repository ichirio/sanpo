#' Treatment-pathway Sankey and sunburst in one call
#'
#' `sp_sankey()` and `sp_sunburst()` go from one-row-per-patient-per-line data
#' straight to a `ggplot`. There is no hidden step: each is the data function
#' followed by the plot function, so
#'
#' ```r
#' sp_sankey(lot)
#' ```
#'
#' is the same as
#'
#' ```r
#' sk <- sp_sankey_data(lot)
#' sp_plot_sankey(sk$nodes, sk$links, node_label = "label", node_value = "n",
#'                node_treatment = "category")
#' ```
#'
#' and `sp_sunburst(lot)` is `sp_plot_sunburst(sp_sunburst_data(lot))`.
#'
#' Nodes are laid out from the node table, not inferred from the links, so a
#' node that no patient enters or leaves -- for example patients whose only
#' "line" is no treatment -- is still drawn at its own size.
#'
#' @inheritParams sp_sankey_data
#' @param ... Further arguments passed to [sp_plot_sankey()] or
#'   [sp_plot_sunburst()] (e.g. `treatment_palette`, `palette`,
#'   `label_size`).
#' @return A `ggplot` object. The table it was drawn from (the
#'   [sp_sankey_data()] list, or the [sp_sunburst_data()] path table) is kept
#'   in `attr(, "sp_data")`.
#' @seealso [sp_sankey_data()], [sp_plot_sankey()], [sp_plot_sunburst()].
#' @examples
#' lot <- sp_synth_pathways(n = 200)
#' sp_sankey(lot)
#' sp_sunburst(lot, center_label = "N = {n}")
#' @export
sp_sankey <- function(data, id = "patient_id", stage = "line", category = "drug_class",
                      levels = NULL, stage_prefix = "L", ...) {
  sk <- sp_sankey_data(data, id = id, stage = stage, category = category,
                       levels = levels, stage_prefix = stage_prefix)
  p <- sp_plot_sankey(sk$nodes, sk$links, node_label = "label", node_value = "n",
                      node_treatment = "category", ...)
  attr(p, "sp_data") <- sk
  p
}

#' @rdname sp_sankey
#' @param center_label Optional text in the centre hole; `"{n}"` is replaced
#'   by the number of patients.
#' @export
sp_sunburst <- function(data, id = "patient_id", stage = "line", category = "drug_class",
                        levels = NULL, stage_prefix = "L", center_label = NULL, ...) {
  paths <- sp_sunburst_data(data, id = id, stage = stage, category = category,
                            levels = levels, stage_prefix = stage_prefix)
  if (!is.null(center_label)) {
    center_label <- gsub("{n}", format(sum(paths$n)), center_label, fixed = TRUE)
  }
  p <- sp_plot_sunburst(paths, path_cols = setdiff(names(paths), "n"), path_value = "n",
                        levels = levels, center_label = center_label, ...)
  attr(p, "sp_data") <- paths
  p
}
