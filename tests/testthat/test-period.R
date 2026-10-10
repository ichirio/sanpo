test_that("sp_month reads dates and YYYYMM", {
  expect_equal(sp_month(c(201904, 202001)), as.Date(c("2019-04-01", "2020-01-01")))
  expect_equal(sp_month("2019-04"), as.Date("2019-04-01"))
  expect_equal(sp_month(as.Date("2019-04-17")), as.Date("2019-04-01"))
  expect_error(sp_month(201913), "YYYYMM")
  expect_error(sp_month("abc"), "YYYYMM")
})

test_that("fiscal years start in April by default", {
  expect_equal(sp_fiscal_year(c(201903, 201904, 202003)), c(2018L, 2019L, 2019L))
  expect_equal(sp_fiscal_year(c(201912, 202001), start = 1), c(2019L, 2020L))
  expect_error(sp_fiscal_year(201904, start = 13), "start")
})

test_that("age bands are 5 years with an open top band", {
  b <- sp_age_band(c(0, 4, 5, 84, 85, 103))
  expect_s3_class(b, "factor")
  expect_equal(as.character(b), c("0-4", "0-4", "5-9", "80-84", "85+", "85+"))
  expect_equal(nlevels(b), 18)
  b10 <- sp_age_band(c(3, 99, 100), width = 10, top = 100)
  expect_equal(as.character(b10), c("0-9", "90-99", "100+"))
  expect_equal(levels(sp_age_band(c(1, 12), top = Inf)), c("0-4", "5-9", "10-14"))
  expect_error(sp_age_band(1, top = 83), "multiple")
  expect_error(sp_age_band(-1), ">= 0")
  expect_error(sp_age_band("a"), "numeric")
})
