ep_small <- function() {
  data.frame(
    patient_id = c("P1", "P1", "P1", "P1", "P2", "P2", "P3"),
    state = c("A", "A", "B", "A", "B", "B", "A"),
    start = as.Date(c("2022-01-01", "2022-02-05", "2022-03-20", "2022-06-01",
                      "2022-01-10", "2022-05-01", "2022-02-01")),
    end = as.Date(c("2022-01-30", "2022-03-06", "2022-04-18", "2022-06-30",
                    "2022-02-08", "2022-05-30", "2022-02-28")),
    stringsAsFactors = FALSE
  )
}

# A data frame copied into an in-memory DuckDB, as a lazy table.
duck_tbl <- function(df, env = parent.frame()) {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  df$state <- as.character(df$state)
  dplyr::copy_to(con, df, "episodes", overwrite = TRUE)
}

quiet <- function(expr) suppressMessages(expr)

# Engine messages on (the test setup silences them).
messages_on <- function(env = parent.frame()) {
  withr::local_options(stats::setNames(list(TRUE), opt_name("engine_messages")), .local_envir = env)
}

lv5 <- paste("Drug", LETTERS[1:5])

test_that("memory and DuckDB give the same journey, nodes, flows, tree and table", {
  ep <- jn_simulate(400)
  db <- duck_tbl(ep)
  args <- list(
    list(),
    list(gap_state = "(gap)", max_steps = 5),
    list(collapse = FALSE, max_steps = Inf, end_state = NULL),
    list(grace = 0, max_steps = 2)
  )
  for (a in args) {
    jm <- quiet(do.call(jn_sequence, c(list(ep, states = lv5, engine = "memory"), a)))
    jd <- quiet(do.call(jn_sequence, c(list(db, states = lv5, engine = "db"), a)))
    expect_true(dplyr::is.tbl(jd@sequence) && inherits(jd@sequence, "tbl_lazy"))
    expect_equal(quiet(jn_collect(jd))@sequence, jm@sequence)
    expect_identical(jd@states, jm@states)
    expect_identical(jd@meta, jm@meta)
    expect_equal(jd@n_persons, jm@n_persons)
    expect_identical(quiet(jn_flows(jd, engine = "db")), quiet(jn_flows(jm)))
    expect_identical(quiet(jn_tree(jd, engine = "db")), quiet(jn_tree(jm)))
    expect_identical(quiet(tbl_pathways(jd, engine = "db", min_cell = 5)),
                     quiet(tbl_pathways(jm, min_cell = 5)))
    expect_identical(summary(jd), summary(jm))
  }
  # month data and steps subsets
  em <- jn_simulate(200, unit = "month")
  dbm <- duck_tbl(em)
  jm <- quiet(jn_sequence(em, unit = "month", states = lv5, gap_state = "(gap)"))
  jd <- quiet(jn_sequence(dbm, unit = "month", states = lv5, gap_state = "(gap)", engine = "db"))
  expect_equal(quiet(jn_collect(jd))@sequence, jm@sequence)
  expect_identical(quiet(jn_flows(jd, steps = c(1, 3), engine = "db")), quiet(jn_flows(jm, steps = c(1, 3))))
  expect_identical(quiet(tbl_pathways(jd, steps = 2:3, engine = "db")), quiet(tbl_pathways(jm, steps = 2:3)))
})

test_that("plots read a journey whose sequence is in the database", {
  ep <- jn_simulate(200)
  db <- duck_tbl(ep)
  jm <- quiet(jn_sequence(ep, states = lv5, max_steps = 3))
  jd <- quiet(jn_sequence(db, states = lv5, max_steps = 3, engine = "db"))
  pm <- quiet(plot_sankey(jm))
  pd <- quiet(plot_sankey(jd))
  expect_equal(ggplot2::layer_data(pd, 1), ggplot2::layer_data(pm, 1))
  expect_equal(quiet(plot_sunburst(jd))$data, quiet(plot_sunburst(jm))$data)
  expect_output(print(jd), "in the database")
})

test_that("auto collects a small lazy table and stays in the database above the limit", {
  messages_on()
  ep <- ep_small()
  db <- duck_tbl(ep)
  expect_message(j <- jn_sequence(db, states = c("A", "B", "C")), "engine = memory")
  expect_false(inherits(j@sequence, "tbl_lazy"))
  # input over the limit, sequence too: everything stays in the database
  expect_message(j2 <- jn_sequence(db, states = c("A", "B", "C"), memory_limit = 100),
                 "engine = db")
  expect_true(inherits(j2@sequence, "tbl_lazy"))
  expect_equal(quiet(jn_collect(j2))@sequence, j@sequence)
  # input over the limit, sequence under it: switch to memory after the build
  sim <- jn_simulate(100)
  dbs <- duck_tbl(sim)
  big <- jn_size(dbs)$bytes
  msgs <- testthat::capture_messages(
    j3 <- jn_sequence(dbs, states = lv5, max_steps = 2, memory_limit = big - 1)
  )
  expect_match(msgs[1], "engine = db")
  expect_match(msgs[2], "collected")
  expect_false(inherits(j3@sequence, "tbl_lazy"))
  expect_equal(j3@sequence, quiet(jn_sequence(sim, states = lv5, max_steps = 2))@sequence)
  # the global option drives the default
  old <- options(stats::setNames(list(100), opt_name("memory_limit")))
  on.exit(options(old), add = TRUE)
  expect_message(j4 <- jn_sequence(db, states = c("A", "B", "C")), "engine = db")
  expect_true(inherits(j4@sequence, "tbl_lazy"))
  expect_message(jn_flows(j4), "engine = db")
})

test_that("collecting above the limit is refused with a clear message", {
  ep <- ep_small()
  db <- duck_tbl(ep)
  expect_error(quiet(jn_sequence(db, engine = "memory", memory_limit = "0.5KB")),
               class = "jn_memory_limit_error")
  expect_error(quiet(jn_sequence(db, engine = "memory", memory_limit = 10)), "raise the limit")
  jd <- quiet(jn_sequence(db, states = c("A", "B", "C"), engine = "db"))
  expect_error(jn_collect(jd, memory_limit = 10), class = "jn_memory_limit_error")
  expect_error(jn_flows(jd, engine = "memory", memory_limit = 10), class = "jn_memory_limit_error")
  expect_error(jn_sequence(ep, engine = "db"), "lazy")
})

test_that("every call names the engine and the size estimate", {
  messages_on()
  ep <- ep_small()
  expect_message(j <- jn_sequence(ep), "jn_sequence\\(\\): engine = memory \\(data frame, 7 rows, about")
  expect_message(jn_flows(j), "jn_flows\\(\\): engine = memory")
  expect_message(jn_tree(j), "jn_tree")
  expect_message(tbl_pathways(j), "tbl_pathways")
  expect_message(plot_sankey(j), "plot_sankey")
  old <- options(stats::setNames(list(FALSE), opt_name("engine_messages")))
  on.exit(options(old), add = TRUE)
  expect_silent(jn_flows(j))
})

test_that("sizes are parsed and estimated from rows x column types", {
  expect_equal(parse_bytes("2GB"), 2 * 1024^3)
  expect_equal(parse_bytes("500 mb"), 500 * 1024^2)
  expect_equal(parse_bytes(1000), 1000)
  expect_error(parse_bytes("lots"), "size")
  sz <- jn_size(ep_small())
  # patient_id + state (character, 64) + start + end (Date, 8)
  expect_equal(sz$bytes, 7 * (64 + 64 + 8 + 8))
  expect_equal(sz$limit, 2 * 1024^3)
  expect_false(sz$lazy)
  expect_equal(format_bytes(2 * 1024^3), "2.0 GB")
})
