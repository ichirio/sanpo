test_that("the grid has 47 prefectures on distinct tiles", {
  g <- sp_pref_grid()
  expect_equal(nrow(g), 47)
  expect_equal(g$code, 1:47)
  expect_false(anyDuplicated(paste(g$col, g$row)) > 0)
  expect_false(anyDuplicated(g$abbr) > 0)
  expect_equal(g$name_en[13], "Tokyo")
})

test_that("prefectures are matched by code or name", {
  d <- data.frame(pref = c("01", "13", "Kyoto", "Osaka-fu", "東京都", "京都"),
                  v = 1:6)
  expect_error(sp_pref_tile_data(d, pref = "pref", value = "v"), "more than one row")
  d <- d[c(1, 3, 4), ]
  tl <- sp_pref_tile_data(d, pref = "pref", value = "v")
  expect_equal(nrow(tl), 47)
  expect_equal(tl$value[c(1, 26, 27)], c(1, 3, 4))
  expect_equal(sum(!is.na(tl$value)), 3)
  tl_ja <- sp_pref_tile_data(data.frame(p = c("東京都", "北海道"), v = 1:2),
                             pref = "p", value = "v")
  expect_equal(tl_ja$value[c(1, 13)], c(2, 1))
})

test_that("bad input gives clear errors", {
  expect_error(sp_pref_tile_data(data.frame(pref_code = 48, value = 1)), "not prefectures")
  expect_error(sp_pref_tile_data(data.frame(pref_code = "Atlantis", value = 1)), "Atlantis")
  expect_error(sp_pref_tile_data(data.frame(pref_code = 1, value = "a")), "numeric")
  expect_error(sp_pref_tile_data(data.frame(pref_code = 1)), "value")
})

test_that("sp_pref_tile draws 47 tiles", {
  p <- sp_pref_tile(sp_synth_pref(), value = "rate")
  expect_s3_class(p, "ggplot")
  expect_equal(geoms(p), c("GeomTile", "GeomText"))
  expect_equal(nrow(ggplot2::layer_data(p, 1)), 47)
  q <- sp_pref_tile(sp_synth_pref(), value = "rate", label = "code", palette = "grey")
  expect_equal(ggplot2::layer_data(q, 2)$label[1], "01")
})
