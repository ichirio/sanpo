#' Synthetic treatment pathways
#'
#' Make a small, entirely made-up treatment-sequence data set for examples
#' and tests: one row per patient and treatment line. Nothing in it comes
#' from a real database; drug classes are placeholders (`"Class A"` to
#' `"Class E"`).
#'
#' About 15% of the patients have a single row with `drug_class =
#' "No treatment"` at line 1 and no later line, so a Sankey of the data has
#' a node with no links. A treated patient has one to four lines, and a new
#' line always switches to a different class.
#'
#' @param n Number of patients.
#' @param seed Random seed, or `NULL` to use the current random state. The
#'   global random state is restored on exit when a seed is given.
#' @return A data frame with columns `patient_id` (character), `sex`
#'   (`"M"` / `"F"`), `line` (integer, from 1) and `drug_class` (character),
#'   sorted by patient and line.
#' @examples
#' lot <- sp_synth_pathways(n = 100)
#' head(lot)
#' table(lot$line)
#' @export
sp_synth_pathways <- function(n = 500, seed = 1) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    stop("`n` must be a single number >= 1.", call. = FALSE)
  }
  n <- as.integer(n)
  if (!is.null(seed)) {
    genv <- globalenv()
    seed_var <- ".Random.seed"
    old <- if (exists(seed_var, envir = genv, inherits = FALSE)) get(seed_var, envir = genv) else NULL
    on.exit({
      if (is.null(old)) {
        rm(list = seed_var, envir = genv)
      } else {
        assign(seed_var, old, envir = genv)
      }
    }, add = TRUE)
    set.seed(seed)
  }

  classes <- paste("Class", LETTERS[1:5])
  prob <- c(0.35, 0.25, 0.18, 0.12, 0.10)
  ids <- sprintf(paste0("P%0", max(4L, nchar(n)), "d"), seq_len(n))
  sex <- sample(c("M", "F"), n, replace = TRUE)
  untreated <- stats::runif(n) < 0.15
  n_lines <- pmin(1L + stats::rgeom(n, 0.45), 4L)

  rows <- lapply(seq_len(n), function(i) {
    if (untreated[i]) {
      cls <- "No treatment"
    } else {
      cls <- character(n_lines[i])
      cls[1] <- sample(classes, 1, prob = prob)
      for (k in seq_len(n_lines[i])[-1]) {
        pool <- classes != cls[k - 1]
        cls[k] <- sample(classes[pool], 1, prob = prob[pool])
      }
    }
    data.frame(patient_id = ids[i], sex = sex[i], line = seq_along(cls),
               drug_class = cls, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
