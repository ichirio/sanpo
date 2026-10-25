j_demo <- function() jn_sequence(jn_simulate(300), max_steps = 3)

layer_of <- function(p, geom) {
  i <- which(vapply(p$layers, function(l) inherits(l$geom, geom), logical(1)))
  ggplot2::layer_data(p, i[1])
}

test_that("plot_sankey draws aggregated flows and empty nodes", {
  j <- j_demo()
  p <- plot_sankey(j)
  expect_s3_class(p, "ggplot")
  rects <- layer_of(p, "GeomRect")
  # 4 drugs + empty Drug E at each of 3 steps, plus (end) at steps 2 and 3
  expect_equal(nrow(rects), 3 * 5 + 2)
  poly <- layer_of(p, "GeomPolygon")
  fl <- jn_flows(j)
  expect_equal(length(unique(poly$group)), sum(fl$n > 0))
  q <- plot_sankey(j, empty = "hide", label = "none")
  expect_equal(nrow(layer_of(q, "GeomRect")), 3 * 4 + 2)
  expect_s3_class(plot_sankey(j, steps = 1:2), "ggplot")
  # the hidden legend keys must not widen the step axis
  expect_gt(ggplot2::layer_scales(p)$x$range$range[1], 0.5)
})

test_that("sunburst and icicle share the tree rectangles", {
  j <- j_demo()
  s <- plot_sunburst(j)
  i <- plot_icicle(j)
  expect_s3_class(s, "ggplot")
  expect_s3_class(s$coordinates, "CoordRadial")
  tr <- jn_tree(j)
  expect_equal(nrow(s$data), nrow(tr))
  expect_equal(s$data$start, i$data$start)
  expect_s3_class(plot_sunburst(j, steps = 2, label = "state"), "ggplot")
  expect_equal(max(plot_icicle(j, steps = 2)$data$depth), 2)
})

test_that("scale_fill_journey gives the same colours everywhere", {
  j <- j_demo()
  sc <- scale_fill_journey(j)
  expect_equal(sc$limits, j@states$level)
  fill_of <- function(p, geom) {
    d <- layer_of(p, geom)
    unique(d$fill)
  }
  all_cols <- j@states$colour
  expect_true(all(fill_of(plot_sankey(j), "GeomRect") %in% all_cols))
  expect_true(all(fill_of(plot_sunburst(j), "GeomRect") %in% all_cols))
})

test_that("tbl_pathways counts pathways and suppresses small cells", {
  j <- j_demo()
  tb <- tbl_pathways(j)
  expect_equal(sum(tb$n), 300)
  expect_true(all(diff(tb$n) <= 0))
  expect_false(any(grepl("(end)", tb$pathway, fixed = TRUE)))
  te <- tbl_pathways(j, include_end = TRUE)
  expect_true(any(grepl("(end)", te$pathway, fixed = TRUE)))
  ts <- tbl_pathways(j, min_cell = 10)
  expect_true(all(is.na(ts$n[ts$suppressed])))
  expect_true(all(ts$n_label[ts$suppressed] == "<10"))
  expect_true(all(ts$n[!ts$suppressed] >= 10))
  expect_error(tbl_pathways(j, min_cell = 0))
  expect_equal(max(tbl_pathways(j, steps = 1)$n_steps), 1)
})
