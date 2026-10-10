test_that("sp_save writes PNG, TIFF and SVG", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) + ggplot2::geom_point()
  for (ext in c("png", "tiff", "svg")) {
    f <- tempfile(fileext = paste0(".", ext))
    expect_equal(sp_save(p, f, size = "single", dpi = 100), f)
    expect_true(file.exists(f))
    expect_gt(file.size(f), 0)
  }
  f <- tempfile(fileext = ".png")
  sp_save(p, f, width = 50, height = 40, dpi = 100)
  sig <- readBin(f, "raw", 24)
  w <- sum(as.integer(sig[17:20]) * 256^(3:0))
  expect_equal(w, round(50 / 25.4 * 100), tolerance = 1)
})

test_that("sp_save rejects unknown formats", {
  p <- ggplot2::ggplot()
  expect_error(sp_save(p, tempfile(fileext = ".pdf")), "png")
  expect_error(sp_save(p, tempfile(fileext = ".png"), dpi = 10), "dpi")
})
