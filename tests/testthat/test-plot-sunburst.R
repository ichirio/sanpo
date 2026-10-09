test_that("sp_plot_sunburst lays out nested arcs with attrition gaps", {
  paths <- data.frame(
    l1 = c("A", "A", "A", "B"),
    l2 = c("B", "A", NA, NA),
    n = c(5, 2, 3, 10),
    stringsAsFactors = FALSE
  )

  p <- sp_plot_sunburst(paths, path_cols = c("l1", "l2"), path_value = "n")
  expect_s3_class(p, "ggplot")

  rect <- ggplot2::layer_data(p, 1)
  expect_equal(nrow(rect), 4)

  ring1 <- rect[rect$ymin == min(rect$ymin), ]
  ring1 <- ring1[order(ring1$xmin), ]
  # Ring 1: A [0, 10], B [10, 20] (levels default to appearance order).
  expect_equal(ring1$xmin, c(0, 10), tolerance = 1e-8)
  expect_equal(ring1$xmax, c(10, 20), tolerance = 1e-8)

  ring2 <- rect[rect$ymin == max(rect$ymin), ]
  ring2 <- ring2[order(ring2$xmin), ]
  # Ring 2 nests from the parent's start: A [0, 2], B [2, 7]; the remaining
  # [7, 10] of parent A stays empty (the n = 3 path that stopped), and
  # parent B has no children at all.
  expect_equal(ring2$xmin, c(0, 2), tolerance = 1e-8)
  expect_equal(ring2$xmax, c(2, 7), tolerance = 1e-8)
})

test_that("sp_plot_sunburst sums duplicate paths and hides sliver labels", {
  paths <- data.frame(
    l1 = c("A", "A", "B"),
    n = c(30, 60, 10),
    stringsAsFactors = FALSE
  )

  p <- sp_plot_sunburst(paths, path_cols = "l1", path_value = "n",
                     label_min_frac = 0.2)
  rect <- ggplot2::layer_data(p, 1)
  expect_equal(nrow(rect), 2)
  expect_equal(sort(rect$xmax - rect$xmin), c(10, 90), tolerance = 1e-8)

  txt <- ggplot2::layer_data(p, 2)
  # A (90/100) is labelled with its count; B (10/100) is below label_min_frac.
  expect_true(any(grepl("90", txt$label)))
  expect_true(any(txt$label == ""))
})

test_that("sp_plot_sunburst validates paths", {
  expect_error(
    sp_plot_sunburst(
      data.frame(l1 = "A", l2 = "B", n = -1, stringsAsFactors = FALSE),
      path_cols = c("l1", "l2")
    ),
    "non-negative"
  )

  # A path may not resume after an NA ring.
  expect_error(
    sp_plot_sunburst(
      data.frame(
        l1 = c("A", NA),
        l2 = c("B", "A"),
        n = c(1, 1),
        stringsAsFactors = FALSE
      ),
      path_cols = c("l1", "l2")
    ),
    "resume"
  )

  expect_error(
    sp_plot_sunburst(
      data.frame(l1 = c("A", "B"), n = c(1, 1), stringsAsFactors = FALSE),
      path_cols = "l1",
      levels = "A"
    ),
    "missing from `levels`"
  )
})

test_that("sp_plot_sunburst honours palette, labels and center annotation", {
  paths <- data.frame(
    l1 = c("A", "B"),
    n = c(6, 4),
    stringsAsFactors = FALSE
  )

  p <- sp_plot_sunburst(
    paths,
    path_cols = "l1", path_value = "n",
    palette = c(A = "#111111", B = "#222222"),
    segment_labels = c(A = "Alpha"),
    center_label = "N = 10"
  )

  rect <- ggplot2::layer_data(p, 1)
  expect_setequal(rect$fill, c("#111111", "#222222"))

  txt <- ggplot2::layer_data(p, 2)
  expect_true(any(grepl("Alpha", txt$label)))

  # rects + arc labels + centre annotation
  expect_equal(length(p$layers), 3)
})
