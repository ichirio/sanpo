test_that("fills within the gap form one episode", {
  rx <- data.frame(patient_id = c("a", "a", "a", "b"),
                   fill_date = as.Date(c("2021-01-01", "2021-02-10", "2021-06-01", "2021-01-01")),
                   days_supply = c(30, 30, 30, 30))
  ep <- sp_episodes(rx, gap = 30, end_date = as.Date("2021-12-31"), first_only = FALSE)
  a <- ep[ep$patient_id == "a", ]
  expect_equal(a$episode, c(1L, 2L))
  expect_equal(a$n_fills, c(2L, 1L))
  expect_equal(a$end[1], as.Date("2021-03-11"))
  expect_equal(a$discontinued, c(1L, 1L))
  expect_equal(a$duration[1], as.numeric(as.Date("2021-03-11") - as.Date("2021-01-01")) + 1)
  # 10 days without supply (Jan 31 to Feb 9): gap = 10 keeps one episode
  ep2 <- sp_episodes(rx, gap = 10, end_date = as.Date("2021-12-31"))
  expect_equal(ep2$n_fills[ep2$patient_id == "a"], 2L)
  ep3 <- sp_episodes(rx, gap = 9, end_date = as.Date("2021-12-31"))
  expect_equal(ep3$n_fills[ep3$patient_id == "a"], 1L)
})

test_that("supply running to the end of data is censored", {
  rx <- data.frame(patient_id = "a", fill_date = as.Date("2021-12-01"), days_supply = 30)
  ep <- sp_episodes(rx, gap = 30, end_date = as.Date("2021-12-31"))
  expect_equal(ep$discontinued, 0L)
  expect_equal(ep$duration, 31)
})

test_that("bad input gives clear errors", {
  rx <- data.frame(patient_id = "a", fill_date = "2021-01-01", days_supply = 30)
  expect_error(sp_episodes(rx), "Date")
  rx$fill_date <- as.Date(rx$fill_date)
  expect_error(sp_episodes(transform(rx, days_supply = 0)), ">= 1")
  expect_error(sp_episodes(rx, days = "days"), "days")
  expect_error(sp_persistence_data(data.frame(duration = 1, discontinued = 2)), "0 / 1")
})

test_that("sp_persistence censors at the horizon and draws a KM curve", {
  ep <- sp_episodes(sp_synth_rx(n = 150), by = "drug")
  km <- sp_persistence_data(ep, group = "drug", horizon = 180)
  expect_lte(max(km$curve$time), 180)
  expect_equal(km$risk$time[km$risk$group == "Drug A"], c(0, 30, 60, 90, 120, 150, 180))
  p <- sp_persistence(ep, group = "drug")
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$caption, "^Median: Drug A")
  expect_equal(p$labels$y, "Persistent on treatment")
})
