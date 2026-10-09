test_that("sp_synth_pathways has the documented shape", {
  d <- sp_synth_pathways(n = 200)
  expect_named(d, c("patient_id", "sex", "line", "drug_class"))
  expect_equal(length(unique(d$patient_id)), 200)
  expect_type(d$line, "integer")
  expect_true(all(d$line >= 1 & d$line <= 4))
  expect_setequal(unique(d$sex), c("M", "F"))
  expect_false(anyDuplicated(d[c("patient_id", "line")]) > 0)

  # untreated patients have one row only
  none <- d$patient_id[d$drug_class == "No treatment"]
  expect_true(length(none) > 0)
  expect_true(all(table(d$patient_id)[none] == 1))
  expect_true(all(d$line[d$drug_class == "No treatment"] == 1))

  # a new line always switches class
  same <- d$patient_id[-1] == d$patient_id[-nrow(d)] & d$drug_class[-1] == d$drug_class[-nrow(d)]
  expect_false(any(same))
})

test_that("sp_synth_pathways is reproducible and leaves the RNG alone", {
  set.seed(42)
  before <- .Random.seed
  a <- sp_synth_pathways(n = 50, seed = 7)
  expect_identical(.Random.seed, before)
  expect_identical(a, sp_synth_pathways(n = 50, seed = 7))
  expect_false(identical(a, sp_synth_pathways(n = 50, seed = 8)))
  expect_error(sp_synth_pathways(n = 0), "`n`")
})
