test_that("sp_pyramid_data counts by 5-year band and sex", {
  d <- data.frame(age = c(3, 7, 7, 86, 90), sex = c("M", "F", "2", "male", "1"))
  py <- sp_pyramid_data(d)
  expect_equal(names(py), c("age_band", "sex", "n", "pct"))
  expect_equal(nrow(py), 18 * 2)
  expect_equal(py$n[py$age_band == "5-9" & py$sex == "F"], 2)
  expect_equal(py$n[py$age_band == "85+" & py$sex == "M"], 2)
  expect_equal(sum(py$pct), 100)
  expect_equal(nlevels(sp_pyramid_data(d, top = 75)$age_band), 16)
})

test_that("counted data is used as is", {
  tab <- data.frame(band = rep(c("0-39", "40+"), 2), sex = c("M", "M", "F", "F"),
                    n = c(1, 2, 3, 4))
  py <- sp_pyramid_data(tab, age = "band", count = "n")
  expect_equal(levels(py$age_band), c("0-39", "40+"))
  expect_equal(py$n, c(1, 3, 2, 4))
})

test_that("bad sex codes and ages are errors", {
  expect_error(sp_pyramid_data(data.frame(age = 1, sex = "X")), "not male / female")
  expect_error(sp_pyramid_data(data.frame(age = -1, sex = "M")), ">= 0")
  expect_error(sp_pyramid_data(data.frame(age = "1", sex = "M")), "numeric")
  expect_error(sp_pyramid_data(data.frame(age = 1, sex = NA)), "missing")
  expect_error(sp_pyramid_data(data.frame(years = 1, sex = "M")), "years|age")
})

test_that("males are drawn left, females right", {
  d <- data.frame(age = c(30, 31, 32, 50), sex = c("M", "M", "F", "F"))
  p <- sp_pyramid(d)
  expect_s3_class(p, "ggplot")
  ld <- ggplot2::layer_data(p, 1)
  expect_equal(sum(ld$x[ld$x < 0]), -2)
  expect_equal(sum(ld$x[ld$x > 0]), 2)
  expect_equal(attr(p, "sp_data"), sp_pyramid_data(d))
  q <- sp_pyramid(d, percent = TRUE, lang = "ja")
  expect_equal(sum(abs(ggplot2::layer_data(q, 1)$x)), 100)
  expect_equal(q$labels$y, sp_label("age_group", "ja"))
})
