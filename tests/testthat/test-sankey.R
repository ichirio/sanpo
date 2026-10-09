# Nodes laid out from the node table: a node no patient enters or leaves is
# still drawn, at its own size.
node_rects <- function(p) {
  is_rect <- vapply(p$layers, function(l) inherits(l$geom, "GeomRect"), logical(1))
  ggplot2::layer_data(p, which(is_rect)[1])
}

test_that("sp_sankey is sp_sankey_data + sp_plot_sankey", {
  lot <- sp_synth_pathways(n = 120)
  p <- sp_sankey(lot)
  expect_s3_class(p, "ggplot")
  sk <- sp_sankey_data(lot)
  expect_equal(attr(p, "sp_data"), sk)
  q <- sp_plot_sankey(sk$nodes, sk$links, node_label = "label", node_value = "n",
                      node_treatment = "category")
  expect_equal(node_rects(p), node_rects(q))
  expect_equal(nrow(node_rects(p)), nrow(sk$nodes))
})

test_that("a node without links is drawn at its own size (one-call path)", {
  lot <- data.frame(
    patient_id = c("1", "1", "2", "2", "3", "4", "5"),
    line       = c(1, 2, 1, 2, 1, 1, 1),
    drug_class = c("A", "B", "A", "B", "None", "None", "None")
  )
  p <- sp_sankey(lot, show_labels = FALSE, node_gap = 0, node_min_size = 0)
  sk <- attr(p, "sp_data")
  none <- sk$nodes$id[sk$nodes$category == "None"]
  expect_equal(sk$nodes$n[sk$nodes$id == none], 3)
  expect_false(any(c(sk$links$source, sk$links$target) == none))

  rect <- node_rects(p)
  expect_equal(nrow(rect), 3)  # L1: A, L1: None, L2: B
  heights <- rect$ymax - rect$ymin
  # L1: A (2 patients) and L1: None (3 patients, no links) in input order
  expect_equal(heights[rect$xmin < 0.5], c(2, 3), tolerance = 1e-8)
})

test_that("a stage made only of unlinked nodes is drawn", {
  nodes <- data.frame(id = c("A", "B", "C"), stage = c("S1", "S2", "S3"),
                      n = c(5, 3, 2), stringsAsFactors = FALSE)
  links <- data.frame(source = character(), target = character(), value = numeric())
  p <- sp_plot_sankey(nodes, links, node_value = "n", show_labels = FALSE,
                      node_min_size = 0)
  expect_equal(length(p$layers), 1)  # no link polygons at all
  rect <- ggplot2::layer_data(p, 1)
  expect_equal(nrow(rect), 3)
  expect_equal(rect$ymax - rect$ymin, c(5, 3, 2), tolerance = 1e-8)
})

test_that("the synthetic data gives an unlinked 'No treatment' node", {
  p <- sp_sankey(sp_synth_pathways(n = 300))
  sk <- attr(p, "sp_data")
  none <- sk$nodes[sk$nodes$category == "No treatment", ]
  expect_equal(nrow(none), 1)
  expect_gt(none$n, 0)
  expect_false(none$id %in% c(sk$links$source, sk$links$target))
})

test_that("sp_sunburst is sp_sunburst_data + sp_plot_sunburst", {
  lot <- sp_synth_pathways(n = 150)
  p <- sp_sunburst(lot, center_label = "N = {n}")
  expect_s3_class(p, "ggplot")
  paths <- sp_sunburst_data(lot)
  expect_equal(attr(p, "sp_data"), paths)
  expect_equal(sum(paths$n), 150)
  expect_equal(ggplot2::layer_data(p, 3)$label, "N = 150")
  q <- sp_plot_sunburst(paths)
  expect_equal(ggplot2::layer_data(p, 1), ggplot2::layer_data(q, 1))
})
