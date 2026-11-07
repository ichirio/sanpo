# The 2026-11-07 decision: data preparation takes a data frame or a lazy
# table with the same result; plots take a journey (aggregated data) and no
# engine arguments. See DECISIONS.md.

prep_verbs <- c("jn_sequence", "jn_flows", "jn_tree", "tbl_pathways",
                "journey", "jn_collect", "jn_size", "jn_lines", "jn_attrition",
                "jn_matrix", "jn_grid")
# plots that take a journey; plot_attrition takes the table of jn_attrition()
plot_verbs <- c("plot_sankey", "plot_sunburst", "plot_icicle", "plot_transition",
                "plot_lasagna")
helpers <- c("jn_states", "jn_simulate", "jn_suppress", "scale_fill_journey",
             "scale_colour_journey", "theme_journey", "class_journey",
             "plot_attrition")

test_that("every exported function is classified as data prep, plot or helper", {
  exports <- getNamespaceExports(pkg_name())
  expect_equal(sort(setdiff(exports, c(prep_verbs, plot_verbs, helpers))),
               character(0))
  expect_equal(sort(setdiff(c(prep_verbs, plot_verbs, helpers), exports)),
               character(0))
})

test_that("plots have no engine arguments", {
  for (f in c(plot_verbs, "plot_attrition")) {
    expect_false(any(c("engine", "memory_limit") %in% names(formals(get(f)))), info = f)
  }
})

test_that("jn_sequence gives the same journey for a data frame and a lazy table", {
  ep <- jn_simulate(250)
  db <- duck_tbl(ep)
  ref <- quiet(jn_sequence(ep, states = lv5, gap_state = "(gap)", max_steps = 4))
  for (eng in c("auto", "memory", "db")) {
    jd <- quiet(jn_sequence(db, states = lv5, gap_state = "(gap)", max_steps = 4,
                            engine = eng))
    expect_equal(quiet(jn_collect(jd))@sequence, ref@sequence, info = eng)
    expect_identical(jd@states, ref@states, info = eng)
    expect_identical(jd@meta, ref@meta, info = eng)
  }
  # no `states`: the sorted unique values give the same order for both
  ep_chr <- ep
  ep_chr$state <- as.character(ep_chr$state)
  expect_equal(quiet(jn_collect(quiet(jn_sequence(db, engine = "db"))))@sequence,
               quiet(jn_sequence(ep_chr))@sequence)
  # a data frame with a factor state keeps its levels; the lazy table needs
  # `states` for the same order and empty nodes
  ep$state <- factor(ep$state, levels = lv5)
  expect_identical(quiet(jn_sequence(ep))@states$level,
                   quiet(jn_sequence(db, states = lv5))@states$level)
  # month data
  em <- jn_simulate(150, unit = "month")
  expect_equal(
    quiet(jn_collect(quiet(jn_sequence(duck_tbl(em), unit = "month", states = lv5, engine = "db"))))@sequence,
    quiet(jn_sequence(em, unit = "month", states = lv5))@sequence
  )
})

test_that("jn_sequence checks a lazy table like a data frame", {
  ep <- ep_small()
  db <- duck_tbl(ep)
  expect_error(quiet(jn_sequence(db, state = "nope")), "not found")
  expect_error(quiet(jn_sequence(db, grace = -1)), "grace")
  expect_error(quiet(jn_sequence(db, states = c("A", "C"))), "not in")
  bad <- ep
  bad$end[1] <- bad$start[1] - 1
  expect_error(quiet(jn_sequence(duck_tbl(bad))), "before")
  expect_error(quiet(jn_sequence(bad)), "before")
})

test_that("journey() takes a data frame or a lazy table", {
  ep <- jn_simulate(100)
  jm <- quiet(jn_sequence(ep, states = lv5, max_steps = 3))
  seq_df <- as.data.frame(jm@sequence)
  seq_df$state <- as.character(seq_df$state)
  jl <- journey(duck_tbl(seq_df), states = jm@states, meta = jm@meta)
  expect_true(inherits(jl@sequence, "tbl_lazy"))
  expect_equal(jl@n_persons, jm@n_persons)
  expect_equal(quiet(jn_collect(jl))@sequence, jm@sequence)
  expect_identical(quiet(jn_flows(jl, engine = "db")), quiet(jn_flows(jm)))
  expect_identical(summary(jl), summary(jm))
  # the same checks as for a data frame
  expect_error(journey(duck_tbl(seq_df), states = c("Drug A")), "not in")
  expect_error(journey(duck_tbl(seq_df[, c("patient_id", "step")])), "columns")
  expect_error(journey(1:3), "columns")
})

test_that("journey verbs give the same result on a data frame and a lazy sequence", {
  ep <- jn_simulate(300)
  db <- duck_tbl(ep)
  jm <- quiet(jn_sequence(ep, states = lv5, max_steps = 4))
  jd <- quiet(jn_sequence(db, states = lv5, max_steps = 4, engine = "db"))
  for (eng in c("auto", "memory", "db")) {
    expect_identical(quiet(jn_flows(jd, engine = eng)), quiet(jn_flows(jm)), info = eng)
    expect_identical(quiet(jn_tree(jd, engine = eng)), quiet(jn_tree(jm)), info = eng)
    expect_identical(quiet(tbl_pathways(jd, engine = eng)), quiet(tbl_pathways(jm)), info = eng)
  }
  expect_identical(quiet(jn_flows(jd, steps = c(1, 3), engine = "db")),
                   quiet(jn_flows(jm, steps = c(1, 3))))
  expect_identical(quiet(jn_tree(jd, depth = 2, engine = "db")), quiet(jn_tree(jm, depth = 2)))
  expect_identical(quiet(tbl_pathways(jd, include_end = TRUE, sep = " > ", engine = "db")),
                   quiet(tbl_pathways(jm, include_end = TRUE, sep = " > ")))
  # jn_collect(): a journey in memory is returned as it is; jn_size() counts both
  expect_identical(jn_collect(jm), jm)
  sd <- jn_size(jd)
  sm <- jn_size(jm)
  expect_true(sd$lazy && !sm$lazy)
  expect_equal(sd$rows, sm$rows)
  expect_equal(jn_size(db)$rows, jn_size(ep)$rows)
})

test_that("plots read a lazy-sequence journey and draw the same figure", {
  ep <- jn_simulate(200)
  jm <- quiet(jn_sequence(ep, states = lv5, max_steps = 3))
  jd <- quiet(jn_sequence(duck_tbl(ep), states = lv5, max_steps = 3, engine = "db"))
  for (f in plot_verbs) {
    pm <- quiet(get(f)(jm))
    pd <- quiet(get(f)(jd))
    expect_s3_class(pd, "ggplot")
    expect_equal(ggplot2::layer_data(pd, 1), ggplot2::layer_data(pm, 1), info = f)
  }
  # over the limit nothing is collected but the aggregate
  old <- options(stats::setNames(list(100), opt_name("memory_limit")))
  on.exit(options(old), add = TRUE)
  expect_equal(ggplot2::layer_data(quiet(plot_icicle(jd)), 1),
               ggplot2::layer_data(quiet(plot_icicle(jm)), 1))
})

test_that("a lazy table or a data frame is not a plot input: clear message", {
  ep <- jn_simulate(50)
  db <- duck_tbl(ep)
  for (f in plot_verbs) {
    expect_error(get(f)(db), "lazy table", info = f)
    expect_error(get(f)(db), "jn_sequence", info = f)
    expect_error(get(f)(ep), "data frame", info = f)
  }
  expect_error(jn_flows(db), "jn_sequence")
  expect_error(jn_tree(db), "jn_sequence")
  expect_error(tbl_pathways(ep), "jn_sequence")
})

test_that("jn_lines gives the same lines for a data frame and a lazy table", {
  ep <- jn_simulate(250)
  db <- duck_tbl(ep)
  ref <- quiet(jn_lines(ep, drug = "state", drugs = lv5))
  for (eng in c("auto", "memory", "db")) {
    jd <- quiet(jn_lines(db, drug = "state", drugs = lv5, engine = eng))
    expect_equal(quiet(jn_collect(jd))@sequence, ref@sequence, info = eng)
    expect_identical(jd@states, ref@states, info = eng)
    expect_identical(jd@meta, ref@meta, info = eng)
  }
  # drugs not given: sorted unique values for both
  ep_chr <- ep
  ep_chr$state <- as.character(ep_chr$state)
  expect_equal(quiet(jn_collect(quiet(jn_lines(duck_tbl(ep_chr), drug = "state", engine = "db"))))@sequence,
               quiet(jn_lines(ep_chr, drug = "state"))@sequence)
  # an index date column, window and grace
  ep$index <- as.Date("2022-03-01")
  expect_equal(
    quiet(jn_collect(quiet(jn_lines(duck_tbl(ep), drug = "state", drugs = lv5, index = "index",
                                    window = 14, grace = 45, engine = "db"))))@sequence,
    quiet(jn_lines(ep, drug = "state", drugs = lv5, index = "index", window = 14, grace = 45))@sequence
  )
  # month data
  em <- jn_simulate(150, unit = "month")
  expect_equal(
    quiet(jn_collect(quiet(jn_lines(duck_tbl(em), drug = "state", drugs = lv5, unit = "month", engine = "db"))))@sequence,
    quiet(jn_lines(em, drug = "state", drugs = lv5, unit = "month"))@sequence
  )
  # same checks
  expect_error(quiet(jn_lines(db, drug = "state", drugs = lv5[1:2])), "not in")
  expect_error(quiet(jn_lines(db, drug = "nope")), "not found")
})

test_that("jn_attrition gives the same table for a data frame and a lazy table", {
  ep <- jn_simulate(300)
  db <- duck_tbl(ep)
  crit <- list("Drug A to C" = quote(state %in% c("Drug A", "Drug B", "Drug C")),
               "Supply >= 28 days" = quote(end - start >= 27))
  run <- function(d, ...) {
    quiet(eval(as.call(c(list(quote(jn_attrition), d), crit, list(...)))))
  }
  ref <- run(ep)
  for (eng in c("auto", "memory", "db")) {
    expect_identical(run(db, engine = eng), ref, info = eng)
  }
  expect_identical(run(db, min_cell = 150, engine = "db"), run(ep, min_cell = 150))
  # another id column
  ep2 <- ep
  names(ep2)[1] <- "pid"
  expect_identical(
    quiet(jn_attrition(duck_tbl(ep2), id = "pid", "Drug A" = state == "Drug A")),
    quiet(jn_attrition(ep2, id = "pid", "Drug A" = state == "Drug A"))
  )
  expect_error(quiet(jn_attrition(db, state == "Drug A")), "name")
  expect_error(quiet(jn_attrition(db, "x" = state == "Drug A", id = "nope")), "not found")
})

test_that("jn_matrix and jn_grid give the same result on a data frame and a lazy sequence", {
  ep <- jn_simulate(300)
  jm <- quiet(jn_sequence(ep, states = lv5, max_steps = 4))
  jd <- quiet(jn_sequence(duck_tbl(ep), states = lv5, max_steps = 4, engine = "db"))
  for (eng in c("auto", "memory", "db")) {
    expect_identical(quiet(jn_matrix(jd, engine = eng)), quiet(jn_matrix(jm)), info = eng)
    expect_identical(quiet(jn_grid(jd, n_max = 80, engine = eng)),
                     quiet(jn_grid(jm, n_max = 80)), info = eng)
  }
  expect_identical(quiet(jn_matrix(jd, by_step = TRUE, min_cell = 10, engine = "db")),
                   quiet(jn_matrix(jm, by_step = TRUE, min_cell = 10)))
  expect_identical(quiet(jn_matrix(jd, steps = c(1, 3), engine = "db")),
                   quiet(jn_matrix(jm, steps = c(1, 3))))
  expect_identical(quiet(jn_grid(jd, horizon = 300, engine = "db")),
                   quiet(jn_grid(jm, horizon = 300)))
  # a lines journey in the database behaves the same way
  lm <- quiet(jn_lines(ep, drug = "state", drugs = lv5))
  ld <- quiet(jn_lines(duck_tbl(ep), drug = "state", drugs = lv5, engine = "db"))
  expect_identical(quiet(jn_matrix(ld, engine = "db")), quiet(jn_matrix(lm)))
  expect_identical(quiet(tbl_pathways(ld, engine = "db")), quiet(tbl_pathways(lm)))
})

test_that("the family plots read a lazy-sequence journey and draw the same figure", {
  ep <- jn_simulate(200)
  jm <- quiet(jn_sequence(ep, states = lv5, max_steps = 3))
  jd <- quiet(jn_sequence(duck_tbl(ep), states = lv5, max_steps = 3, engine = "db"))
  for (f in c("plot_transition", "plot_lasagna")) {
    pm <- quiet(get(f)(jm))
    pd <- quiet(get(f)(jd))
    expect_equal(ggplot2::layer_data(pd, 1), ggplot2::layer_data(pm, 1), info = f)
  }
  # plot_attrition takes the small table, never the data
  at <- quiet(jn_attrition(duck_tbl(ep), "Drug A" = state == "Drug A", engine = "db"))
  expect_s3_class(plot_attrition(at), "ggplot")
  expect_error(plot_attrition(duck_tbl(ep)), "jn_attrition")
  expect_error(plot_attrition(ep), "jn_attrition")
})
