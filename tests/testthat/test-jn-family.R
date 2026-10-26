quiet <- function(expr) suppressMessages(expr)

duck_tbl <- function(df, env = parent.frame()) {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  for (k in names(df)) if (is.factor(df[[k]])) df[[k]] <- as.character(df[[k]])
  dplyr::copy_to(con, df, "episodes", overwrite = TRUE)
}

lv5 <- paste("Drug", LETTERS[1:5])

rx <- function() {
  # P1: A, then B added (A continues) -> A, A + B
  # P2: A, switched to C (A stops within the window) -> A, C
  # P3: A + B started together, gap > grace, A + B again -> one line
  # P4: B, gap, A -> B, A
  data.frame(
    patient_id = c("P1", "P1", "P1", "P1", "P2", "P2", "P3", "P3", "P3", "P3", "P4", "P4"),
    drug = c("A", "A", "B", "A", "A", "C", "A", "B", "A", "B", "B", "A"),
    start = as.Date(c("2022-01-01", "2022-01-31", "2022-03-01", "2022-03-02",
                      "2022-01-01", "2022-02-25", "2022-01-01", "2022-01-10",
                      "2022-08-01", "2022-08-03", "2022-01-01", "2022-06-01")),
    end = as.Date(c("2022-01-30", "2022-03-01", "2022-04-30", "2022-05-30",
                    "2022-03-01", "2022-04-30", "2022-02-28", "2022-03-10",
                    "2022-09-30", "2022-09-30", "2022-02-28", "2022-07-30")),
    stringsAsFactors = FALSE
  )
}

test_that("jn_lines finds add-ons, switches and same-regimen restarts", {
  lt <- quiet(jn_lines(rx(), drugs = c("A", "B", "C")))
  expect_true(S7::S7_inherits(lt, class_journey))
  s <- lt@sequence
  path <- function(p) as.character(s$state[s$patient_id == p])
  expect_equal(path("P1"), c("A", "A + B", "(end)"))
  expect_equal(path("P2"), c("A", "C", "(end)"))
  expect_equal(path("P3"), c("A + B", "(end)"))
  expect_equal(path("P4"), c("B", "A", "(end)"))
  # regimens ordered by number of drugs, then by drug order
  expect_equal(lt@states$level, c("A", "B", "C", "A + B", "(end)"))
  expect_equal(lt@meta$type, "lines")
  expect_equal(lt@meta$window, 28)
  expect_equal(lt@meta$grace, 60)
  # index column drops earlier rows
  d <- rx()
  d$index <- as.Date("2022-02-01")
  li <- quiet(jn_lines(d, drugs = c("A", "B", "C"), index = "index"))
  expect_equal(as.character(li@sequence$state[li@sequence$patient_id == "P4"]), c("A", "(end)"))
  expect_error(quiet(jn_lines(rx(), drugs = c("A", "B"))), "not in")
})

test_that("jn_lines gives the same lines in memory and in DuckDB", {
  ep <- jn_simulate(300)
  db <- duck_tbl(ep)
  lm <- quiet(jn_lines(ep, drug = "state", drugs = lv5))
  ld <- quiet(jn_lines(db, drug = "state", drugs = lv5, engine = "db"))
  expect_equal(quiet(jn_collect(ld))@sequence, lm@sequence)
  expect_identical(ld@states, lm@states)
  # every single drug is a state, unused ones as empty nodes
  expect_true(all(lv5 %in% lm@states$level))
  expect_equal(sum(lm@sequence$state == "Drug E"), 0)
  expect_identical(quiet(tbl_pathways(ld, engine = "db")), quiet(tbl_pathways(lm)))
  expect_s3_class(quiet(plot_sankey(lm)), "ggplot")
})

test_that("jn_matrix and plot_transition count consecutive steps", {
  j <- quiet(jn_sequence(jn_simulate(300), max_steps = 4))
  m <- quiet(jn_matrix(j))
  lv <- j@states$level
  expect_equal(nrow(m), (length(lv) - 1) * length(lv))
  fl <- quiet(jn_flows(j))
  expect_equal(sum(m$n), sum(fl$n))
  tot <- tapply(m$pct, m$from, sum)
  expect_true(all(abs(tot[!is.na(tot) & tot > 0] - 1) < 1e-12))
  expect_equal(sum(m$n[m$to == "Drug E"]), 0)
  mb <- quiet(jn_matrix(j, by_step = TRUE))
  expect_equal(sort(unique(mb$step_from)), 1:3)
  ms <- quiet(jn_matrix(j, min_cell = 10))
  expect_true(all(is.na(ms$n[ms$suppressed])))
  expect_true(all(ms$n_label[ms$suppressed] == "<10"))
  p <- quiet(plot_transition(j))
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(ggplot2::layer_data(p, 1)), nrow(m))
  expect_s3_class(quiet(plot_transition(j, by_step = TRUE, label = "pct")), "ggplot")
  db <- duck_tbl(jn_simulate(300))
  jd <- quiet(jn_sequence(db, states = lv5, max_steps = 4, engine = "db"))
  expect_identical(quiet(jn_matrix(jd, engine = "db")), m)
})

test_that("jn_grid sorts by pathway and down-samples the same way in DuckDB", {
  ep <- jn_simulate(500)
  j <- quiet(jn_sequence(ep, states = lv5, max_steps = 4))
  g <- quiet(jn_grid(j, n_max = 100))
  expect_equal(attr(g, "every"), 5)
  expect_equal(attr(g, "n_shown"), 100)
  expect_equal(attr(g, "n_persons"), 500)
  expect_true(all(g$from[g$step == 1] == 0))
  expect_true(all(g$to > g$from))
  # rows follow the pathway order of step 1
  s1 <- g[g$step == 1, ]
  expect_false(is.unsorted(as.integer(s1$state[order(s1$row)])))
  full <- quiet(jn_grid(j))
  expect_equal(attr(full, "every"), 1)
  expect_equal(length(unique(full$row)), 500)
  gh <- quiet(jn_grid(j, n_max = 50, horizon = 365))
  expect_true(max(gh$to) <= 365)
  db <- duck_tbl(ep)
  jd <- quiet(jn_sequence(db, states = lv5, max_steps = 4, engine = "db"))
  expect_identical(quiet(jn_grid(jd, n_max = 100, engine = "db")), g)
  p <- quiet(plot_lasagna(j, n_max = 100))
  expect_s3_class(p, "ggplot")
  expect_true(all(ggplot2::layer_data(p, 1)$fill %in% j@states$colour))
  expect_s3_class(quiet(plot_lasagna(j, show_end = FALSE, horizon = 400)), "ggplot")
})

test_that("jn_attrition counts remaining and excluded patients", {
  ep <- jn_simulate(400)
  at <- quiet(jn_attrition(ep,
    "Drug A to C" = state %in% c("Drug A", "Drug B", "Drug C"),
    "Supply >= 28 days" = end - start >= 27
  ))
  expect_s3_class(at, "jn_attrition")
  expect_equal(at$n[1], 400)
  expect_equal(at$n[2], length(unique(ep$patient_id[ep$state %in% c("Drug A", "Drug B", "Drug C")])))
  expect_equal(at$excluded[-1], -diff(at$n))
  expect_true(all(diff(at$n) <= 0))
  db <- duck_tbl(ep)
  ad <- quiet(jn_attrition(db,
    "Drug A to C" = state %in% c("Drug A", "Drug B", "Drug C"),
    "Supply >= 28 days" = end - start >= 27, engine = "db"
  ))
  expect_identical(ad, at)
  as <- quiet(jn_attrition(ep, "Few" = patient_id %in% c("P00001", "P00002"), min_cell = 5))
  expect_true(is.na(as$n[2]))
  expect_equal(as$n_label[2], "<5")
  expect_s3_class(plot_attrition(at), "ggplot")
  expect_error(jn_attrition(ep, state == "Drug A"), "name")
})

test_that("the suppression hook: zeros kept, option and custom rule", {
  d <- data.frame(n = c(25L, 3L, 0L), pct = c(0.9, 0.1, 0))
  s <- jn_suppress(d, 10, pct = "pct")
  expect_equal(s$suppressed, c(FALSE, TRUE, FALSE))
  expect_equal(s$n_label, c("25", "<10", "0"))
  expect_true(is.na(s$pct[2]))
  expect_identical(jn_suppress(d), d)
  withr::local_options(stats::setNames(list(10), opt_name("min_cell")))
  j <- quiet(jn_sequence(jn_simulate(200), max_steps = 3))
  tb <- quiet(tbl_pathways(j))
  expect_true(all(tb$n[!tb$suppressed] >= 10))
  withr::local_options(stats::setNames(list(function(x, min_cell, cols, pct) {
    x$custom <- TRUE
    x
  }), opt_name("suppress")))
  expect_true(all(quiet(jn_matrix(j))$custom))
})
