test_that("exact Poisson interval matches poisson.test", {
  d <- data.frame(period = 1:3, events = c(0, 5, 120), person_years = c(1000, 2000, 50000))
  r <- sp_rate_data(d, per = 1)
  for (i in 1:3) {
    ci <- stats::poisson.test(d$events[i], d$person_years[i])$conf.int
    expect_equal(c(r$lower[i], r$upper[i]), as.numeric(ci), tolerance = 1e-8)
  }
  expect_equal(r$rate, d$events / d$person_years)
})

test_that("Byar and normal intervals are close to exact for large counts", {
  d <- data.frame(period = 1, events = 400, person_years = 1e5)
  ex <- sp_rate_data(d, per = 1e5)
  by <- sp_rate_data(d, per = 1e5, ci = "byar")
  no <- sp_rate_data(d, per = 1e5, ci = "normal")
  expect_equal(by$lower, ex$lower, tolerance = 0.01)
  expect_equal(no$upper, ex$upper, tolerance = 0.02)
})

test_that("prevalence uses the binomial interval", {
  d <- data.frame(period = 1, cases = 7, n = 50)
  r <- sp_rate_data(d, events = "cases", denom = "n", type = "prevalence", per = 100)
  ci <- stats::binom.test(7, 50)$conf.int * 100
  expect_equal(c(r$lower, r$upper), as.numeric(ci), tolerance = 1e-8)
  expect_error(sp_rate_data(d, events = "cases", denom = "n", type = "prevalence", ci = "byar"),
               "incidence")
  expect_error(sp_rate_data(transform(d, cases = 60), events = "cases", denom = "n",
                            type = "prevalence"), "exceed")
})

test_that("bad input gives clear errors", {
  d <- data.frame(period = 1, events = 1, person_years = 10)
  expect_error(sp_rate_data(d, events = "n"), "\"n\" \\(`events`\\)")
  expect_error(sp_rate_data(transform(d, events = -1)), ">= 0")
  expect_error(sp_rate_data(transform(d, events = 1.5)), "whole")
  expect_error(sp_rate_data(transform(d, person_years = 0)), "zeros")
  expect_error(sp_rate_data(transform(d, events = "1")), "numeric")
})

test_that("sp_rate draws points with interval bars", {
  r <- sp_synth_rates()
  p <- sp_rate(r, by = "sex")
  expect_s3_class(p, "ggplot")
  expect_true(all(c("GeomErrorbar", "GeomLine", "GeomPoint") %in% geoms(p)))
  ld <- ggplot2::layer_data(p, which(geoms(p) == "GeomErrorbar"))
  tab <- attr(p, "sp_data")
  expect_equal(sort(ld$ymax), sort(tab$upper))
  expect_match(p$labels$y, "per 100,000 person-years")
  q <- sp_rate(r[r$sex == "M", ], ribbon = TRUE)
  expect_true("GeomRibbon" %in% geoms(q))
})
