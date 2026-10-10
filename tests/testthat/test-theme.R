test_that("palette, theme and labels", {
  expect_equal(sp_palette(3), c("#0072B2", "#D55E00", "#009E73"))
  expect_length(sp_palette(4, "grey"), 4)
  expect_length(sp_palette(5, "viridis"), 5)
  expect_warning(sp_palette(9), "recycling")
  expect_s3_class(sp_theme(), "theme")
  expect_equal(sp_label(c("patients", "fiscal_year")), c("Patients", "Fiscal year"))
  expect_equal(sp_label("male", "ja"), "男性")
  expect_equal(sp_label("unknown_key"), "unknown_key")
  expect_error(sp_label("male", "fr"), "lang")
})

test_that("the default language comes from an option", {
  withr_lang <- function(code) {
    old <- options(sanpo.lang = "ja")
    on.exit(options(old))
    code
  }
  expect_equal(withr_lang(sp_label("female")), "女性")
  expect_equal(sp_label("female"), "Female")
})
