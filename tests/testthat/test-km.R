test_that("Kaplan-Meier curve matches survfit", {
  s <- sp_synth_survival(n = 200)
  km <- sp_km_data(s, group = "group")
  expect_s3_class(km, "sp_km")
  expect_equal(km$type, "km")
  fit <- survival::survfit(survival::Surv(time, status) ~ group, data = s)
  sm <- summary(fit, times = c(100, 300))
  for (k in seq_along(sm$time)) {
    g <- sub("group=", "", as.character(sm$strata[k]))
    cv <- km$curve[km$curve$group == g & km$curve$time <= sm$time[k], ]
    expect_equal(utils::tail(cv$estimate, 1), sm$surv[k], tolerance = 1e-10)
  }
  expect_true(all(km$curve$estimate[km$curve$time == 0] == 1))
  expect_equal(nrow(km$median), 2)
})

test_that("number at risk counts patients still followed", {
  d <- data.frame(time = c(1, 2, 3, 4, 10), status = c(1, 0, 1, 1, 0))
  km <- sp_km_data(d, times = c(0, 2, 5))
  expect_equal(km$risk$n_risk, c(5, 4, 1))
  expect_false(km$grouped)
})

test_that("competing risks give Aalen-Johansen cumulative incidence", {
  s <- sp_synth_survival(n = 300)
  km <- sp_km_data(s, status = "status_cr", group = "group")
  expect_equal(km$type, "cuminc")
  expect_setequal(unique(km$curve$state), c("1", "2"))
  fit <- survival::survfit(survival::Surv(time, factor(status_cr)) ~ 1,
                           data = s[s$group == "Drug A", ])
  last <- utils::tail(km$curve[km$curve$group == "Drug A" & km$curve$state == "1", ], 1)
  expect_equal(last$estimate, utils::tail(fit$pstate[, 2], 1), tolerance = 1e-10)
  # the 1 - KM of the event alone overstates it
  km1 <- sp_km_data(s[s$group == "Drug A", ])
  expect_lt(last$estimate, 1 - utils::tail(km1$curve$estimate, 1))
  f <- sp_km_data(transform(s, status_cr = factor(status_cr, 0:2, c("cens", "event", "death"))),
                  status = "status_cr")
  expect_setequal(unique(f$curve$state), c("event", "death"))
})

test_that("bad input gives clear errors", {
  d <- data.frame(time = c(1, 2), status = c(1, 0))
  expect_error(sp_km_data(transform(d, time = c(-1, 2))), ">= 0")
  expect_error(sp_km_data(transform(d, status = c(0.5, 1))), "0 / 1")
  expect_error(sp_km_data(transform(d, status = c(NA, 1))), "missing")
  expect_error(sp_km_data(d, group = "arm"), "arm")
  expect_warning(sp_km_data(transform(d, status = 0)), "No events")
  expect_error(sp_plot_km(d), "sp_km_data")
})

test_that("sp_km draws steps, bands and the risk table", {
  s <- sp_synth_survival(n = 200)
  p <- sp_km(s, group = "group")
  expect_s3_class(p, "ggplot")
  expect_equal(geoms(p), c("GeomRibbon", "GeomStep"))
  ld <- ggplot2::layer_data(p, 2)
  expect_equal(nrow(ld), nrow(attr(p, "sp_data")$curve))
  q <- sp_km(s, group = "group", cumulative = TRUE, ci = FALSE)
  expect_equal(geoms(q), "GeomStep")
  expect_equal(min(ggplot2::layer_data(q, 1)$y), 0)
  rt <- sp_plot_risk_table(attr(p, "sp_data"))
  expect_equal(geoms(rt), "GeomText")
  skip_if_not_installed("patchwork")
  pw <- sp_km(s, group = "group", risk_table = TRUE)
  expect_s3_class(pw, "patchwork")
  cr <- sp_km(s, status = "status_cr", states = "1")
  expect_equal(unique(ggplot2::layer_data(cr, 2)$y[1]), 0)
  expect_error(sp_km(s, status = "status_cr", states = "3"), "not in the fit")
})
