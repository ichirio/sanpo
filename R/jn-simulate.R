#' Simulate claims-like treatment episodes
#'
#' Make an entirely synthetic data set of prescription rows for examples,
#' tests and timing. Nothing comes from a real database or a real code
#' master: the drug codes are fictional 9-character strings that only
#' imitate the shape of a claims drug code (`"6299"` followed by digits),
#' and the drug classes are `"Drug A"` to `"Drug E"`.
#'
#' Each patient has one to five treatment blocks; a new block switches to a
#' different class. A block is one or more prescriptions of 14 to 60 days
#' separated by short gaps (within a 30-day grace period); between blocks the
#' gap is sometimes long. `"Drug E"` is a level of `state` that no patient
#' receives, so plots of the data show an empty node.
#'
#' @param n Number of patients (1 to 1e6).
#' @param unit `"day"` for dated prescriptions, or `"month"` for claims that
#'   only carry the month (dates floored to the first of the month).
#' @param seed Random seed, or `NULL` to use the current random state. The
#'   global random state is restored on exit when a seed is given.
#' @param from First possible start date.
#' @return A data frame with columns `patient_id`, `code` (fictional),
#'   `state` (factor with levels `"Drug A"` to `"Drug E"`), `start` and `end`
#'   (`Date`), sorted by patient and start.
#' @examples
#' ep <- jn_simulate(50)
#' head(ep)
#' table(ep$state)
#' @export
jn_simulate <- function(n = 1000, unit = c("day", "month"), seed = 1,
                        from = as.Date("2022-04-01")) {
  unit <- match.arg(unit)
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1 || n > 1e6) {
    cli::cli_abort("{.arg n} must be a single number from 1 to 1e6.")
  }
  n <- as.integer(n)
  if (!is.null(seed)) {
    genv <- globalenv()
    seed_var <- ".Random.seed"
    old <- if (exists(seed_var, envir = genv, inherits = FALSE)) get(seed_var, envir = genv) else NULL
    on.exit({
      if (is.null(old)) rm(list = seed_var, envir = genv) else assign(seed_var, old, envir = genv)
    }, add = TRUE)
    set.seed(seed)
  }

  lv <- paste("Drug", LETTERS[1:5])
  prob <- c(0.45, 0.30, 0.15, 0.10, 0)   # Drug E: never used (empty node)
  k <- pmin(1L + stats::rgeom(n, 0.5), 5L)          # blocks per patient
  b_pat <- rep(seq_len(n), k)
  b_idx <- sequence(k)
  b_cls <- sample.int(5, length(b_pat), replace = TRUE, prob = prob)
  for (i in 2:5) {
    at <- which(b_idx == i)
    if (length(at) == 0) next
    prev <- b_cls[at - 1]
    same <- b_cls[at] == prev
    while (any(same)) {
      b_cls[at[same]] <- sample.int(5, sum(same), replace = TRUE, prob = prob)
      same <- b_cls[at] == prev
    }
  }

  m <- 1L + stats::rpois(length(b_pat), 2)          # prescriptions per block
  r_blk <- rep(seq_along(b_pat), m)
  r_pat <- b_pat[r_blk]
  r_cls <- b_cls[r_blk]
  nr <- length(r_blk)
  dur <- sample(c(14L, 28L, 30L, 60L), nr, replace = TRUE, prob = c(0.2, 0.4, 0.3, 0.1))
  first_in_blk <- c(TRUE, r_blk[-1] != r_blk[-nr])
  first_in_pat <- c(TRUE, r_pat[-1] != r_pat[-nr])
  gap <- sample.int(15L, nr, replace = TRUE) - 1L
  long <- first_in_blk & stats::runif(nr) < 0.35
  gap[long] <- 31L + sample.int(150L, sum(long), replace = TRUE)
  gap[first_in_pat] <- sample.int(365L, sum(first_in_pat), replace = TRUE) - 1L
  step <- gap + c(0L, dur[-nr])
  step[first_in_pat] <- gap[first_in_pat]
  cs <- cumsum(as.numeric(step))
  base <- cs[first_in_pat][r_pat] - step[first_in_pat][r_pat]
  start <- from + (cs - base)
  end <- start + dur - 1L

  code_num <- 1000L * r_cls + sample.int(2L, nr, replace = TRUE)
  out <- data.frame(
    patient_id = sprintf(paste0("P%0", max(5L, nchar(n)), "d"), r_pat),
    code = sprintf("6299%05d", code_num),
    state = factor(lv[r_cls], levels = lv),
    start = start,
    end = end,
    stringsAsFactors = FALSE
  )
  if (unit == "month") {
    out$start <- floor_month(out$start)
    out$end <- floor_month(out$end)
  }
  out
}

floor_month <- function(x) as.Date(format(x, "%Y-%m-01"))
