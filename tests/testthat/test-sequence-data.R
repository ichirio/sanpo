lot <- data.frame(
  patient_id = c("1", "1", "2", "3", "3", "3", "4"),
  line       = c(1, 2, 1, 1, 2, 3, 1),
  drug_class = c("Chemo", "IO", "Chemo", "IO", "Chemo", "Targeted", "IO"),
  age        = c("young", "young", "old", "old", "old", "old", "young")
)

test_that("sp_sankey_data counts patients per node and transitions per link", {
  sk <- sp_sankey_data(lot)
  expect_equal(sk$nodes$label, c("L1: Chemo", "L1: IO", "L2: Chemo", "L2: IO", "L3: Targeted"))
  expect_equal(sk$nodes$n, c(2, 2, 1, 1, 1))  # L2: Chemo (patient 3), IO (patient 1)
  expect_equal(sk$links$source, c("S1_Chemo", "S1_IO", "S2_Chemo"))
  expect_equal(sk$links$target, c("S2_IO", "S2_Chemo", "S3_Targeted"))
  expect_equal(sk$links$value, c(1, 1, 1))
  expect_s3_class(sp_plot_sankey(sk$nodes, sk$links, node_label = "label", node_value = "n",
                                 node_treatment = "category"), "ggplot")
})

test_that("sp_sankey_data takes the column names as arguments", {
  cdisc <- stats::setNames(lot, c("USUBJID", "LINE", "TRT", "AGE"))
  expect_equal(sp_sankey_data(cdisc, id = "USUBJID", stage = "LINE", category = "TRT"),
               sp_sankey_data(lot))
})

test_that("sp_sankey_data builds subgroups and respects category levels", {
  sk <- sp_sankey_data(lot, by = "age", levels = c("IO", "Chemo", "Targeted"))
  expect_setequal(unique(sk$nodes$grp), c("ALL", "young", "old"))
  expect_equal(sk$nodes$label[sk$nodes$grp == "ALL"][1:2], c("L1: IO", "L1: Chemo"))
  expect_equal(sum(sk$nodes$n[sk$nodes$grp == "young" & sk$nodes$stage == "L1"]), 2)
})

test_that("sp_sunburst_data gives one row per sequence", {
  p <- sp_sunburst_data(lot)
  expect_equal(names(p), c("L1", "L2", "L3", "n"))
  expect_equal(nrow(p), 4)
  expect_equal(p$n[is.na(p$L2) & p$L1 == "IO"], 1)
  expect_equal(sum(p$n), 4)
  expect_s3_class(sp_plot_sunburst(p), "ggplot")
})

test_that("more than one row per patient and line is an error", {
  expect_error(sp_sankey_data(rbind(lot, lot[1, ])), "More than one row")
  expect_error(sp_sankey_data(lot, id = "nope"), "not found")
})
