# Shared by the engine and stage tests.
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
  if ("state" %in% names(df)) df$state <- as.character(df$state)
  dplyr::copy_to(con, df, "episodes", overwrite = TRUE)
}

quiet <- function(expr) suppressMessages(expr)

# Engine messages on (the test setup silences them).
messages_on <- function(env = parent.frame()) {
  withr::local_options(stats::setNames(list(TRUE), opt_name("engine_messages")), .local_envir = env)
}

lv5 <- paste("Drug", LETTERS[1:5])
