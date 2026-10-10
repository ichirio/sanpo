test_that("sp_plot_sankey_batch applies shared scale across subgroups", {
  nodes <- data.frame(
    id = c("A", "B"),
    stage = c("L1", "L2"),
    stringsAsFactors = FALSE
  )

  links <- data.frame(
    source = c("A", "A"),
    target = c("B", "B"),
    value = c(100, 50),
    scenario = c("full", "sub"),
    stringsAsFactors = FALSE
  )

  specs <- data.frame(
    subgroup = c("full", "sub"),
    link_filter = c("scenario == 'full'", "scenario == 'sub'"),
    stringsAsFactors = FALSE
  )

  res <- sp_plot_sankey_batch(
    nodes = nodes,
    links = links,
    subgroup_specs = specs,
    scale_strategy = "shared_max",
    save_png = FALSE,
    show_labels = FALSE,
    node_min_size = 0,
    node_gap = 0
  )

  expect_equal(nrow(res$metadata), 2)
  expect_equal(length(unique(res$metadata$shared_scale_max)), 1)

  rect_full <- ggplot2::layer_data(res$plots$full, 2)
  rect_sub <- ggplot2::layer_data(res$plots$sub, 2)

  h_full <- rect_full$ymax[1] - rect_full$ymin[1]
  h_sub <- rect_sub$ymax[1] - rect_sub$ymin[1]
  expect_equal(h_sub / h_full, 0.5, tolerance = 1e-8)
})

test_that("sp_plot_sankey_batch supports first-stage normalization with cap", {
  nodes <- data.frame(
    id = c("A", "B"),
    stage = c("L1", "L2"),
    stringsAsFactors = FALSE
  )

  links <- data.frame(
    source = c("A", "A"),
    target = c("B", "B"),
    value = c(100, 20),
    scenario = c("full", "sub"),
    stringsAsFactors = FALSE
  )

  specs <- data.frame(
    subgroup = c("full", "sub"),
    link_filter = c("scenario == 'full'", "scenario == 'sub'"),
    stringsAsFactors = FALSE
  )

  res_cap <- sp_plot_sankey_batch(
    nodes = nodes,
    links = links,
    subgroup_specs = specs,
    scale_strategy = "first_stage_normalized",
    first_stage_max_multiplier = 2,
    save_png = FALSE,
    show_labels = FALSE,
    node_min_size = 0,
    node_gap = 0
  )

  mul_sub_cap <- res_cap$metadata$multiplier[res_cap$metadata$subgroup == "sub"]
  expect_equal(mul_sub_cap, 2)

  res_nocap <- sp_plot_sankey_batch(
    nodes = nodes,
    links = links,
    subgroup_specs = specs,
    scale_strategy = "first_stage_normalized",
    first_stage_max_multiplier = Inf,
    save_png = FALSE,
    show_labels = FALSE,
    node_min_size = 0,
    node_gap = 0
  )

  mul_sub_nocap <- res_nocap$metadata$multiplier[res_nocap$metadata$subgroup == "sub"]
  expect_equal(mul_sub_nocap, 5)
})

test_that("sp_plot_sankey_batch caps first-stage magnification at 100 by default", {
  expect_equal(formals(sp_plot_sankey_batch)$first_stage_max_multiplier, 100)
})

test_that("sp_plot_sankey_batch supports shared first-stage reference scale", {
  nodes <- data.frame(
    id = c("A", "B", "A", "B"),
    stage = c("L1", "L2", "L1", "L2"),
    node_n = c(100, 300, 50, 150),
    scenario = c("full", "full", "sub", "sub"),
    stringsAsFactors = FALSE
  )

  links <- data.frame(
    source = c("A", "A"),
    target = c("B", "B"),
    value = c(100, 50),
    scenario = c("full", "sub"),
    stringsAsFactors = FALSE
  )

  specs <- data.frame(
    subgroup = c("full", "sub"),
    node_filter = c("scenario == 'full'", "scenario == 'sub'"),
    link_filter = c("scenario == 'full'", "scenario == 'sub'"),
    stringsAsFactors = FALSE
  )

  res <- sp_plot_sankey_batch(
    nodes = nodes,
    links = links,
    subgroup_specs = specs,
    node_value = "node_n",
    scale_strategy = "shared_first_stage",
    scale_reference_subgroup = "full",
    save_png = FALSE,
    show_labels = FALSE,
    node_min_size = 0,
    node_gap = 0
  )

  expect_equal(res$metadata$shared_scale_max[1], 100)
  expect_equal(res$metadata$shared_scale_max[2], 100)
  expect_equal(res$metadata$first_stage_span[1], 100)
  expect_equal(res$metadata$first_stage_span[2], 50)

  rect_full <- ggplot2::layer_data(res$plots$full, 2)
  rect_sub <- ggplot2::layer_data(res$plots$sub, 2)
  h_full <- rect_full$ymax[1] - rect_full$ymin[1]
  h_sub <- rect_sub$ymax[1] - rect_sub$ymin[1]
  expect_equal(h_sub / h_full, 0.5, tolerance = 1e-8)
})
