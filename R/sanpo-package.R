#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data := %||%
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}

# Column names used inside the shared dplyr pipelines (memory and database).
utils::globalVariables(c(
  "patient_id", "state", "start", "end", "step", "t0", "t1", "run",
  "run_end", "prev_end", "prev_state", "new_run", "ord", "cm", "prev", "g0",
  "g1", "new", "grp", "to", "step_to", "same_p", "next_p",
  "new_p", "rn", "first_rn", "block", ".prev", ".next", ".first", "t1_block",
  "new_pd", "pd", "e0", "e1", "prev_e0", "new_line", "line", "c0", "c_last",
  "bit", "code", "rank"
))
