# Visual snapshots. Optional: they run only with 'vdiffr' installed, and only
# on ggplot2 >= 4.0 (the SVG output differs between ggplot2 versions).
skip_if_not_installed("vdiffr")
skip_if(utils::packageVersion("ggplot2") < "4.0.0")
skip_on_os(c("windows", "mac"))

test_that("figures look as expected", {
  vdiffr::expect_doppelganger("pyramid", sp_pyramid(
    data.frame(age = c(5, 25, 25, 47, 63, 88), sex = c("M", "F", "M", "F", "F", "M")), top = 80))
  vdiffr::expect_doppelganger("forest", sp_forest(sp_synth_estimates(), group = "group"))
  vdiffr::expect_doppelganger("pref-tile", sp_pref_tile(sp_synth_pref(), value = "rate"))
  vdiffr::expect_doppelganger("km", sp_km(sp_synth_survival(n = 100), group = "group"))
})
