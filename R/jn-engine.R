#' Where the data verbs run: memory or database
#'
#' Every data verb (`jn_sequence()`, `jn_flows()`, `jn_tree()`,
#' `tbl_pathways()`, ...) accepts a data frame or a lazy database table
#' (a `dbplyr` `tbl_lazy`, for example from DuckDB) and runs the same
#' `dplyr` code on either. The `engine` argument says where the work is
#' done:
#'
#' * `"memory"`: collect the data into R first (refused when the size
#'   estimate is above `memory_limit`).
#' * `"db"`: stay in the database; only the aggregated result (nodes,
#'   flows, pathway counts) is collected.
#' * `"auto"` (default): on a lazy table, count its rows in the database
#'   and estimate its size in memory from rows x column types. Under
#'   `memory_limit` the data are collected and the verb continues in
#'   memory; over it, the verb stays in the database. A data frame always
#'   runs in memory.
#'
#' `jn_sequence()` checks again once the sequence is built: a sequence that
#' fits under the limit is collected, so later steps run in memory. A
#' journey whose sequence stayed in the database can be brought in with
#' [jn_collect()].
#'
#' The size estimate is the size of the collected table: 4 bytes per value
#' for logical, integer and factor columns, 8 for numeric, `Date` and
#' date-time columns and 64 for character columns (pointer plus a share of
#' the string). Working copies made during the computation are not
#' counted, so leave head room.
#'
#' @section Options:
#' * `sanpo.memory_limit`: default for `memory_limit` (`"2GB"`). Set it
#'   once per project so that development (small extracts) and production
#'   (full database) run the same code with only the setting changed, for
#'   example `options(sanpo.memory_limit = "8GB")`.
#' * `sanpo.engine_messages`: `FALSE` silences the message, sent by every
#'   call, that names the engine chosen and the size estimate.
#'
#' The option names start with the package name.
#'
#' @param x A data frame, a lazy table or a [journey()].
#' @param memory_limit Largest estimated size that may be collected into
#'   memory: a number of bytes or a string such as `"500MB"` or `"2GB"`
#'   (units KB, MB, GB, TB of 1024).
#' @return `jn_size()` returns a list with `rows`, `bytes` (estimate),
#'   `lazy` (is `x` a lazy table) and `limit` (bytes).
#' @examples
#' ep <- jn_simulate(100)
#' jn_size(ep)
#' jn_size(ep, memory_limit = "1MB")$limit
#' @name engine
NULL

#' @rdname engine
#' @export
jn_size <- function(x, memory_limit = NULL) {
  if (S7::S7_inherits(x, class_journey)) x <- x@sequence
  rows <- count_rows(x)
  list(rows = rows, bytes = rows * row_bytes(x), lazy = is_lazy(x),
       limit = parse_bytes(memory_limit %||% default_memory_limit()))
}

#' @rdname engine
#' @param j A [journey()].
#' @return `jn_collect()` returns the journey with its sequence in memory.
#' @export
jn_collect <- function(j, memory_limit = NULL) {
  check_journey(j)
  if (!is_lazy(j@sequence)) return(j)
  sz <- jn_size(j, memory_limit)
  check_collect(sz, "jn_collect")
  engine_message("jn_collect", "memory", sz, "collected the sequence")
  seq_to_journey(dplyr::collect(j@sequence), j@states, j@meta)
}

# options ----------------------------------------------------------------------

pkg_name <- function() utils::packageName(environment(pkg_name)) %||% "sanpo"

opt_name <- function(x) paste0(pkg_name(), ".", x)

default_memory_limit <- function() getOption(opt_name("memory_limit"), "2GB")

# "2GB" -> bytes (1024 based); numbers are bytes.
parse_bytes <- function(x) {
  if (is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0) return(as.numeric(x))
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg memory_limit} must be a number of bytes or a string such as {.val 2GB}.")
  }
  ux <- toupper(x)
  m <- regmatches(ux, regexec("^\\s*([0-9.]+)\\s*([KMGT]?B?)\\s*$", ux))[[1]]
  if (length(m) == 0 || is.na(suppressWarnings(as.numeric(m[2])))) {
    cli::cli_abort("{.arg memory_limit} {.val {x}} is not a size such as {.val 500MB} or {.val 2GB}.")
  }
  unit <- substr(m[3], 1, 1)
  pow <- match(unit, c("K", "M", "G", "T"), nomatch = 0)
  as.numeric(m[2]) * 1024^pow
}

format_bytes <- function(b) {
  u <- c("B", "KB", "MB", "GB", "TB")
  k <- if (b <= 0) 0 else min(floor(log(b, 1024)), 4)
  if (k == 0) return(paste0(format(round(b), big.mark = ","), " B"))
  sprintf("%.1f %s", b / 1024^k, u[k + 1])
}

# size estimate -----------------------------------------------------------------

is_lazy <- function(x) inherits(x, "tbl_lazy")

count_rows <- function(x) {
  if (!is_lazy(x)) return(nrow(x))
  as.numeric(dplyr::pull(dplyr::collect(dplyr::count(dplyr::ungroup(x)))))
}

# Zero-row prototype of x in R (one query that returns no rows for a lazy
# table).
col_proto <- function(x) {
  if (!is_lazy(x)) return(x[0, , drop = FALSE])
  dplyr::collect(utils::head(dplyr::ungroup(x), 0))
}

row_bytes <- function(x) {
  p <- col_proto(x)
  b <- vapply(p, function(v) {
    if (is.factor(v) || is.logical(v) || is.integer(v)) 4
    else if (is.character(v)) 64
    else 8
  }, numeric(1))
  sum(b)
}

# engine choice -------------------------------------------------------------------

# Decide where a verb runs. Returns the data to use (collected when the
# verb runs in memory) and the decision. `fn` names the verb in messages.
choose_engine <- function(x, engine, memory_limit, fn) {
  engine <- match.arg(engine, c("auto", "db", "memory"))
  lazy <- is_lazy(x)
  if (!lazy && engine == "db") {
    cli::cli_abort(c("{.code engine = \"db\"} needs a lazy database table.",
                     i = "{.fn {fn}} got a data frame; use {.code engine = \"memory\"} or {.code \"auto\"}."),
                   call = NULL)
  }
  sz <- jn_size(x, memory_limit)
  use <- if (!lazy) "memory" else if (engine == "auto") {
    if (sz$bytes <= sz$limit) "memory" else "db"
  } else engine
  if (lazy && use == "memory") {
    check_collect(sz, fn)
    x <- dplyr::collect(x)
  }
  if (!lazy || use == "memory") x <- as.data.frame(x, stringsAsFactors = FALSE)
  engine_message(fn, use, sz, if (lazy && use == "memory") "collected" else NULL)
  list(data = x, engine = use, size = sz)
}

check_collect <- function(sz, fn) {
  if (sz$bytes > sz$limit) {
    o <- opt_name("memory_limit")
    cli::cli_abort(c(
      "{.fn {fn}}: refusing to collect {format(sz$rows, big.mark = ',')} rows (about {format_bytes(sz$bytes)}) into memory; the limit is {format_bytes(sz$limit)}.",
      i = "Aggregate in the database with {.code engine = \"db\"} (or {.code \"auto\"}), filter the cohort first, or raise the limit with {.arg memory_limit} or {.code options({o} = \"8GB\")}."
    ), call = NULL, class = "jn_memory_limit_error")
  }
  invisible(TRUE)
}

engine_message <- function(fn, use, sz, note = NULL) {
  if (isFALSE(getOption(opt_name("engine_messages"), TRUE))) return(invisible())
  src <- if (sz$lazy) "lazy table" else "data frame"
  txt <- sprintf("%s(): engine = %s (%s, %s rows, about %s; limit %s)%s",
                 fn, use, src, format(sz$rows, big.mark = ","),
                 format_bytes(sz$bytes), format_bytes(sz$limit),
                 if (is.null(note)) "" else paste0("; ", note))
  rlang::inform(txt, class = "jn_engine_message")
}

# shared dplyr helpers ----------------------------------------------------------

# Order rows for window functions (lag, cumsum, row_number) inside groups:
# `window_order()` on a lazy table, `arrange()` in memory. Both give the
# same windows.
win_order <- function(x, ...) {
  if (is_lazy(x)) dbplyr::window_order(x, ...) else dplyr::arrange(x, ..., .by_group = TRUE)
}

# Calendar helpers with the names DuckDB (and most SQL dialects through
# dbplyr) use, so that one dplyr expression runs in R and in the database.
year <- function(x) as.POSIXlt(x)$year + 1900L
month <- function(x) as.POSIXlt(x)$mon + 1L
make_date <- function(year, month, day) {
  n <- max(length(year), length(month), length(day))
  year <- rep_len(year, n)
  month <- rep_len(month, n)
  day <- rep_len(day, n)
  out <- rep(as.Date(NA), n)
  ok <- !is.na(year) & !is.na(month) & !is.na(day)
  out[ok] <- as.Date(sprintf("%04d-%02d-%02d", as.integer(year[ok]),
                             as.integer(month[ok]), as.integer(day[ok])))
  out
}
