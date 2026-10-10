test_that("sp_trend_data counts distinct patients per month and keeps empty months", {
  d <- data.frame(patient_id = c("a", "a", "b", "c", "c"),
                  claim_ym = c(201904, 201904, 201904, 201906, 201907))
  tr <- sp_trend_data(d)
  expect_equal(format(tr$period), c("2019-04-01", "2019-05-01", "2019-06-01", "2019-07-01"))
  expect_equal(tr$n, c(2L, 0L, 1L, 1L))
  expect_equal(tr$label, c("2019-04", "2019-05", "2019-06", "2019-07"))
  expect_equal(sp_trend_data(d, measure = "records")$n, c(3L, 0L, 1L, 1L))
  expect_equal(attr(tr, "unit"), "month")
})

test_that("fiscal years start in April", {
  d <- data.frame(patient_id = c("a", "b", "c", "a"), claim_ym = c(201903, 201904, 202003, 202004))
  tr <- sp_trend_data(d, unit = "fiscal_year")
  expect_equal(tr$period, 2018:2020)
  expect_equal(tr$n, c(1L, 2L, 1L))
  expect_equal(tr$label, c("FY2018", "FY2019", "FY2020"))
  expect_equal(sp_trend_data(d, unit = "year")$n, c(2L, 2L))
})

test_that("groups and rates", {
  d <- data.frame(patient_id = c("a", "b", "c"), sex = c("M", "F", "M"),
                  claim_ym = c(201904, 201904, 201905))
  tr <- sp_trend_data(d, by = "sex")
  expect_equal(nrow(tr), 4)
  expect_equal(tr$n[tr$sex == "F"], c(1L, 0L))
  den <- data.frame(period = c(201904, 201905), denom = c(100, 200))
  r <- sp_trend_data(d, denominator = den, per = 1000)
  expect_equal(r$value, c(2 / 100, 1 / 200) * 1000)
  expect_error(sp_trend_data(d, denominator = den[1, ]), "no `denom`")
  expect_error(sp_trend_data(d, denominator = data.frame(period = 201904)), "denom")
})

test_that("bad input gives clear errors", {
  d <- data.frame(patient_id = "a", claim_ym = 201904)
  expect_error(sp_trend_data(list()), "data frame")
  expect_error(sp_trend_data(d[0, ]), "no rows")
  expect_error(sp_trend_data(d, time = "month"), "\"month\" \\(`time`\\)")
  expect_error(sp_trend_data(d, by = "sex"), "sex")
  expect_error(sp_trend_data(transform(d, claim_ym = 201999)), "YYYYMM")
  expect_error(sp_trend_data(d, unit = "week"), "arg")
})

test_that("sp_trend draws lines and points from the table", {
  cl <- sp_synth_claims(n = 150)
  p <- sp_trend(cl, by = "sex")
  expect_s3_class(p, "ggplot")
  expect_true(all(c("GeomLine", "GeomPoint", "GeomVline") %in% geoms(p)))
  tr <- attr(p, "sp_data")
  expect_equal(tr, sp_trend_data(cl, by = "sex"))
  ld <- ggplot2::layer_data(p, which(geoms(p) == "GeomPoint"))
  expect_equal(sort(ld$y), sort(tr$value))
  q <- sp_trend(cl, unit = "fiscal_year", lang = "ja")
  expect_equal(q$labels$y, sp_label("patients", "ja"))
  expect_false("GeomVline" %in% geoms(q))
})
