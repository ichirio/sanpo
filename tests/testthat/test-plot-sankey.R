test_that("sp_plot_sankey returns ggplot and keeps isolated nodes", {
  nodes <- data.frame(
    id = c("A", "B", "C", "No Treatment"),
    stage = c("L1", "L2", "L2", "L1"),
    label = c("A", "B", "C", "No Treatment"),
    stringsAsFactors = FALSE
  )

  links <- data.frame(
    source = c("A", "A"),
    target = c("B", "C"),
    value = c(10, 5),
    stringsAsFactors = FALSE
  )

  p <- sp_plot_sankey(
    nodes = nodes,
    links = links,
    node_id = "id",
    node_stage = "stage",
    node_label = "label",
    orientation = "horizontal"
  )

  expect_s3_class(p, "ggplot")

  built_nodes <- ggplot2::layer_data(p, length(p$layers) - 1)
  expect_equal(nrow(built_nodes), 4)
  expect_false(any(is.na(built_nodes$xmin)))
  expect_false(any(is.na(built_nodes$ymin)))
})

test_that("sp_plot_sankey supports vertical orientation and bottom baseline", {
  nodes <- data.frame(
    id = c("A", "B", "C"),
    stage = c("S1", "S2", "S2"),
    stringsAsFactors = FALSE
  )

  links <- data.frame(
    source = c("A", "A"),
    target = c("B", "C"),
    value = c(6, 4),
    stringsAsFactors = FALSE
  )

  p <- sp_plot_sankey(
    nodes = nodes,
    links = links,
    orientation = "vertical",
    baseline = "bottom",
    scale_mode = "adaptive",
    shared_scale_max = 20,
    adaptive_max_multiplier = 2
  )

  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 2)
})

test_that("sp_plot_sankey aligns stage tops when baseline is top", {
  nodes <- data.frame(
    id = c("L1_A", "L1_No", "L2_A", "L2_No"),
    stage = c("L1", "L1", "L2", "L2"),
    node_n = c(80, 20, 60, 10),
    stringsAsFactors = FALSE
  )

  links <- data.frame(
    source = c("L1_A"),
    target = c("L2_A"),
    value = c(50),
    stringsAsFactors = FALSE
  )

  p <- sp_plot_sankey(
    nodes = nodes,
    links = links,
    node_value = "node_n",
    baseline = "top",
    show_labels = FALSE
  )

  rect <- ggplot2::layer_data(p, 2)
  # x is stage axis in horizontal orientation; two unique stages around 0 and 1
  l1_top <- max(rect$ymax[rect$xmin < 0.5])
  l2_top <- max(rect$ymax[rect$xmin > 0.5])
  expect_equal(l1_top, l2_top, tolerance = 1e-8)
})

test_that("sp_plot_sankey keeps input order as top-to-bottom when baseline is top", {
  nodes <- data.frame(
    id = c("L1_A", "L1_B", "L1_No", "L2_A"),
    stage = c("L1", "L1", "L1", "L2"),
    label = c("A", "B", "No Treatment", "A2"),
    node_n = c(40, 30, 20, 60),
    stringsAsFactors = FALSE
  )

  links <- data.frame(
    source = c("L1_A", "L1_B"),
    target = c("L2_A", "L2_A"),
    value = c(25, 15),
    stringsAsFactors = FALSE
  )

  p <- sp_plot_sankey(
    nodes = nodes,
    links = links,
    node_label = "label",
    node_value = "node_n",
    baseline = "top",
    show_labels = FALSE
  )

  rect <- ggplot2::layer_data(p, 2)
  l1 <- rect[rect$xmin < 0.5, c("ymin", "ymax")]
  # Third node in input order (No Treatment) should be bottom-most in L1 stack.
  expect_equal(which.min(l1$ymin), 3)
})

test_that("sp_plot_sankey reserves link-width space on the label side of the last stage", {
  nodes <- data.frame(
    id = c("A", "B"),
    stage = c("S1", "S2"),
    label = c("A", "B"),
    stringsAsFactors = FALSE
  )
  links <- data.frame(source = "A", target = "B", value = 10, stringsAsFactors = FALSE)

  node_width <- 0.18
  label_nudge <- 0.03
  reserve <- (1 - node_width) + label_nudge

  # Default: labels on the right; panel extends past the last stage.
  p_right <- sp_plot_sankey(nodes, links, node_label = "label")
  expect_equal(
    p_right$coordinates$limits$x,
    c(0 - node_width / 2, 1 + node_width / 2 + reserve),
    tolerance = 1e-8
  )

  # label_position = "left": space is reserved before the first stage instead.
  p_left <- sp_plot_sankey(nodes, links, node_label = "label", label_position = "left")
  expect_equal(
    p_left$coordinates$limits$x,
    c(0 - node_width / 2 - reserve, 1 + node_width / 2),
    tolerance = 1e-8
  )

  # No labels: no extra space on either side.
  p_off <- sp_plot_sankey(nodes, links, node_label = "label", show_labels = FALSE)
  expect_equal(
    p_off$coordinates$limits$x,
    c(0 - node_width / 2, 1 + node_width / 2),
    tolerance = 1e-8
  )

  # Vertical orientation reserves the space along y.
  p_vert <- sp_plot_sankey(nodes, links, node_label = "label", orientation = "vertical")
  expect_equal(
    p_vert$coordinates$limits$y,
    c(0 - node_width / 2, 1 + node_width / 2 + reserve),
    tolerance = 1e-8
  )
})

test_that("sp_plot_sankey auto node gap scales with label size and shared scale", {
  nodes <- data.frame(
    id = c("A", "B", "C"),
    stage = c("S1", "S1", "S2"),
    label = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  links <- data.frame(
    source = c("A", "B"),
    target = c("C", "C"),
    value = c(100, 50),
    stringsAsFactors = FALSE
  )

  stage_gap <- function(p) {
    rect <- ggplot2::layer_data(p, 2)
    s1 <- rect[rect$xmin < 0.5, ]
    s1 <- s1[order(-s1$ymax), ]
    s1$ymin[1] - s1$ymax[2]
  }

  # Local basis: max raw stage span = 150 -> gap = 0.03 * 3 * 150.
  p_auto <- sp_plot_sankey(nodes, links, node_label = "label")
  expect_equal(stage_gap(p_auto), 0.03 * 3 * 150, tolerance = 1e-8)

  # Shared scale: the gap follows the shared basis, so every plot drawn
  # with the same shared_scale_max gets the identical gap.
  p_shared <- sp_plot_sankey(
    nodes, links,
    node_label = "label",
    scale_mode = "shared", shared_scale_max = 300
  )
  expect_equal(stage_gap(p_shared), 0.03 * 3 * 300, tolerance = 1e-8)

  # An explicit numeric node_gap keeps the fixed behaviour.
  p_fixed <- sp_plot_sankey(nodes, links, node_label = "label", node_gap = 0)
  expect_equal(stage_gap(p_fixed), 0, tolerance = 1e-8)

  # Without labels the automatic gap is a hairline (0.002 * basis).
  p_off <- sp_plot_sankey(nodes, links, node_label = "label", show_labels = FALSE)
  expect_equal(stage_gap(p_off), 0.002 * 150, tolerance = 1e-8)
})

test_that("sp_plot_sankey stacks ribbons from the baseline side in counterpart order", {
  # Target ids chosen so alphabetical order (Y < Z) differs from the input
  # stacking order (Z above Y): the ribbon layout must follow the node
  # positions, not the ids.
  nodes <- data.frame(
    id = c("A", "Z", "Y"),
    stage = c("S1", "S2", "S2"),
    stringsAsFactors = FALSE
  )
  links <- data.frame(
    source = c("A", "A"),
    target = c("Z", "Y"),
    value = c(6, 4),
    stringsAsFactors = FALSE
  )

  # Top-aligned: the ribbon to the top-most target (Z, input-first) leaves
  # from the very top of A and enters the very top of Z (= panel top).
  p_top <- sp_plot_sankey(nodes, links, baseline = "top", show_labels = FALSE)
  poly_top <- ggplot2::layer_data(p_top, 1)
  rect_top <- ggplot2::layer_data(p_top, 2)
  link_z <- poly_top[poly_top$group == 1, ] # poly_id "link_1" = A -> Z
  expect_equal(max(link_z$y), max(rect_top$ymax), tolerance = 1e-8)

  # Bottom-aligned: input-first Z sits at the bottom of S2 and the ribbon to
  # it leaves from the very bottom of A (= 0).
  p_bot <- sp_plot_sankey(nodes, links, baseline = "bottom", show_labels = FALSE)
  poly_bot <- ggplot2::layer_data(p_bot, 1)
  link_z_bot <- poly_bot[poly_bot$group == 1, ]
  expect_equal(min(link_z_bot$y), 0, tolerance = 1e-8)
})

test_that("sp_plot_sankey drops zero-value links without breaking link fills", {
  nodes <- data.frame(
    id = c("A", "B", "C"),
    stage = c("S1", "S2", "S2"),
    stringsAsFactors = FALSE
  )
  links <- data.frame(
    source = c("A", "A"),
    target = c("B", "C"),
    value = c(6, 0),
    fill = c("#123456", "#654321"),
    stringsAsFactors = FALSE
  )

  # The zero-value link is dropped; the surviving link must keep its own
  # fill (the fills were previously resolved against the unfiltered links,
  # which errored on the row-count mismatch).
  p <- sp_plot_sankey(nodes, links, link_fill = "fill", show_labels = FALSE)
  poly <- ggplot2::layer_data(p, 1)
  expect_equal(length(unique(poly$group)), 1)
  expect_equal(unique(poly$fill), "#123456")
})

test_that("sp_plot_sankey draws links in a single light grey by default", {
  nodes <- data.frame(
    id = c("A", "B", "C"),
    stage = c("S1", "S2", "S2"),
    stringsAsFactors = FALSE
  )
  links <- data.frame(
    source = c("A", "A"),
    target = c("B", "C"),
    value = c(6, 4),
    stringsAsFactors = FALSE
  )

  p <- sp_plot_sankey(nodes, links, show_labels = FALSE)
  link_fill <- unique(ggplot2::layer_data(p, 1)$fill)
  expect_equal(link_fill, "#CCCCCC")

  # Nodes carry a black border by default; node_color overrides it.
  expect_equal(unique(ggplot2::layer_data(p, 2)$colour), "black")
  p_none <- sp_plot_sankey(nodes, links, show_labels = FALSE, node_color = NA)
  expect_true(all(is.na(ggplot2::layer_data(p_none, 2)$colour)))
})

test_that("sp_plot_sankey places labels on the requested side", {
  nodes <- data.frame(
    id = c("A", "B"),
    stage = c("S1", "S2"),
    label = c("A", "B"),
    stringsAsFactors = FALSE
  )
  links <- data.frame(source = "A", target = "B", value = 10, stringsAsFactors = FALSE)

  p_right <- sp_plot_sankey(nodes, links, node_label = "label")
  txt_right <- ggplot2::layer_data(p_right, length(p_right$layers))
  # Anchored just right of each node edge (pmax + nudge).
  expect_equal(sort(txt_right$x), c(0.09 + 0.03, 1.09 + 0.03), tolerance = 1e-8)

  p_left <- sp_plot_sankey(nodes, links, node_label = "label", label_position = "left")
  txt_left <- ggplot2::layer_data(p_left, length(p_left$layers))
  # Anchored just left of each node edge (pmin - nudge).
  expect_equal(sort(txt_left$x), c(-0.09 - 0.03, 0.91 - 0.03), tolerance = 1e-8)
})

test_that("sp_plot_sankey validates shared scale input", {
  nodes <- data.frame(id = c("A", "B"), stage = c("S1", "S2"), stringsAsFactors = FALSE)
  links <- data.frame(source = "A", target = "B", value = 1, stringsAsFactors = FALSE)

  expect_error(
    sp_plot_sankey(nodes, links, scale_mode = "shared"),
    "shared_scale_max"
  )
})

test_that("sp_plot_sankey supports treatment color modes", {
  nodes <- data.frame(
    id = c("L1_Chemo", "L1_No", "L2_Chemo", "L2_No"),
    stage = c("L1", "L1", "L2", "L2"),
    line = c("L1", "L1", "L2", "L2"),
    treatment = c("Chemo", "No Treatment", "Chemo", "No Treatment"),
    stringsAsFactors = FALSE
  )

  links <- data.frame(
    source = c("L1_Chemo", "L1_Chemo"),
    target = c("L2_Chemo", "L2_No"),
    value = c(8, 2),
    stringsAsFactors = FALSE
  )

  p_global <- sp_plot_sankey(
    nodes = nodes,
    links = links,
    node_treatment = "treatment",
    node_line = "line",
    treatment_color_mode = "across_lines",
    show_labels = FALSE
  )

  global_fill <- ggplot2::layer_data(p_global, 2)$fill
  expect_equal(global_fill[1], global_fill[3])

  p_by_line <- sp_plot_sankey(
    nodes = nodes,
    links = links,
    node_treatment = "treatment",
    node_line = "line",
    treatment_color_mode = "by_line",
    show_labels = FALSE
  )

  by_line_fill <- ggplot2::layer_data(p_by_line, 2)$fill
  expect_false(identical(by_line_fill[1], by_line_fill[3]))
})