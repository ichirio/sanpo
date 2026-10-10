test_that("sp_forest_data adds text and row order", {
  d <- data.frame(label = c("A", "B"), estimate = c(1.2, 0.8), lower = c(1, 0.5),
                  upper = c(1.5, 1.1))
  fo <- sp_forest_data(d)
  expect_equal(fo$text, c("1.20 (1.00-1.50)", "0.80 (0.50-1.10)"))
  expect_equal(fo$row, 1:2)
})

test_that("broom-style names are recognised", {
  d <- data.frame(term = "x", estimate = 2, conf.low = 1, conf.high = 3)
  fo <- sp_forest_data(d)
  expect_equal(fo$label, "x")
  expect_equal(fo$upper, 3)
})

test_that("groups keep their order and rows are grouped", {
  d <- data.frame(g = c("S", "A", "S"), label = c("m", "x", "f"), estimate = 1, lower = 0.5,
                  upper = 2)
  fo <- sp_forest_data(d, group = "g")
  expect_equal(fo$label, c("m", "f", "x"))
  expect_equal(levels(fo$group), c("S", "A"))
})

test_that("bad input gives clear errors", {
  d <- data.frame(label = "A", estimate = 1, lower = 1.2, upper = 2)
  expect_error(sp_forest_data(d), "lower")
  expect_error(sp_forest_data(d[, -2]), "estimate")
  expect_error(sp_plot_forest(sp_forest_data(transform(d, lower = 0, estimate = 1))), "log")
})

test_that("sp_forest draws estimates on a log axis", {
  est <- sp_synth_estimates()
  p <- sp_forest(est, group = "group")
  expect_s3_class(p, "ggplot")
  expect_true(all(c("GeomVline", "GeomErrorbar", "GeomPoint") %in% geoms(p)))
  ld <- ggplot2::layer_data(p, which(geoms(p) == "GeomPoint"))
  expect_equal(sort(10^ld$x), sort(est$estimate), tolerance = 1e-8)
  expect_s3_class(p$facet, "FacetGrid")
  q <- sp_forest(est, log_scale = FALSE, ref = NULL)
  expect_false("GeomVline" %in% geoms(q))
  expect_equal(sort(ggplot2::layer_data(q, 2)$x), sort(est$estimate))
})
