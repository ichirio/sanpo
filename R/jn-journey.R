#' States dictionary
#'
#' Build the dictionary of states that a journey uses. Its order is the node
#' order in every plot, levels that no patient reaches are kept (they are the
#' empty nodes), and each level has one fixed colour so that the Sankey, the
#' sunburst and the icicle colour a state the same way.
#'
#' Default colours: levels whose name starts with `"("` (such as `"(end)"`
#' or `"(gap)"`) are drawn in greys; the other levels take the Okabe-Ito
#' palette in order (then `grDevices::hcl.colors()` when there are more than
#' seven).
#'
#' @param levels Character vector of state levels, in node order.
#' @param labels Labels shown in plots and tables; defaults to `levels`.
#' @param colours Colours, one per level, or `NULL` for the defaults. A named
#'   vector is matched by level; missing names get the default colour.
#' @return A data frame with columns `level`, `label` and `colour`, of class
#'   `jn_states`.
#' @examples
#' jn_states(c("Drug A", "Drug B", "(end)"))
#' @export
jn_states <- function(levels, labels = levels, colours = NULL) {
  levels <- as.character(levels)
  if (length(levels) == 0 || anyNA(levels) || anyDuplicated(levels)) {
    cli::cli_abort("{.arg levels} must be unique, non-missing strings.")
  }
  if (length(labels) != length(levels)) {
    cli::cli_abort("{.arg labels} must have the same length as {.arg levels}.")
  }
  default <- jn_default_colours(levels)
  if (is.null(colours)) {
    colours <- default
  } else if (!is.null(names(colours))) {
    out <- default
    hit <- intersect(names(colours), levels)
    out[match(hit, levels)] <- colours[hit]
    colours <- out
  } else if (length(colours) != length(levels)) {
    cli::cli_abort("{.arg colours} must have one colour per level.")
  }
  out <- data.frame(level = levels, label = as.character(labels),
                    colour = unname(as.character(colours)),
                    stringsAsFactors = FALSE)
  class(out) <- c("jn_states", "data.frame")
  out
}

jn_default_colours <- function(levels) {
  okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
             "#D55E00", "#CC79A7")
  special <- startsWith(levels, "(")
  k <- sum(!special)
  pal <- if (k <= length(okabe)) okabe[seq_len(k)] else grDevices::hcl.colors(k, "Dark 3")
  greys <- c("grey75", "grey55", "grey40", "grey85")
  out <- character(length(levels))
  out[!special] <- pal
  out[special] <- rep_len(greys, sum(special))
  out
}

as_jn_states <- function(x) {
  if (S7::S7_inherits(x, class_journey)) return(x@states)
  if (inherits(x, "jn_states")) return(x)
  if (is.data.frame(x) && all(c("level", "label", "colour") %in% names(x))) {
    return(jn_states(x$level, x$label, x$colour))
  }
  if (is.character(x)) return(jn_states(x))
  cli::cli_abort("Expected a journey, a {.fn jn_states} dictionary or a character vector of levels.")
}

jn_sequence_cols <- c("patient_id", "step", "state", "start", "end")

#' Journey object
#'
#' A journey is the one data contract that every plot and table in this
#' package reads: the sequence of states per patient, the states dictionary
#' and the parameters that made it. It is usually built by
#' [jn_sequence()]; `journey()` validates a sequence built some other way.
#'
#' A journey is an S7 object of class `class_journey` with the properties
#' `sequence`, `states`, `meta` and the read-only `n_persons`; read them
#' with `@` (`j@sequence`). The class validator keeps the invariants that
#' every plot relies on: the sequence has the required columns, `state` is a
#' factor whose levels are exactly the dictionary's levels (unused levels =
#' empty nodes), and `step` runs 1, 2, 3, ... within each patient. Methods
#' are provided for `print()`, `summary()` (patients per step and state),
#' `as.data.frame()` (the sequence) and `plot()` ([plot_sankey()]). Other
#' packages can add methods for `class_journey` with [S7::method()].
#'
#' @param sequence A data frame with columns `patient_id`, `step` (integer,
#'   from 1 without holes), `state`, `start` and `end`.
#' @param states A [jn_states()] dictionary, a character vector of levels,
#'   or `NULL` to use the levels of `sequence$state` (factor) or its sorted
#'   unique values.
#' @param meta A named list of parameters to keep with the journey.
#' @return `journey()` returns a `class_journey` object.
#' @examples
#' seq <- data.frame(
#'   patient_id = c("P1", "P1", "P2"), step = c(1L, 2L, 1L),
#'   state = c("A", "B", "A"),
#'   start = as.Date(c("2022-01-01", "2022-03-01", "2022-02-01")),
#'   end = as.Date(c("2022-02-01", "2022-04-01", "2022-05-01"))
#' )
#' j <- journey(seq, states = c("A", "B", "C"))
#' j
#' j@n_persons
#' summary(j)
#' @export
journey <- function(sequence, states = NULL, meta = list()) {
  need <- jn_sequence_cols
  if (!is.data.frame(sequence) || !all(need %in% names(sequence))) {
    cli::cli_abort("{.arg sequence} must be a data frame with columns {.val {need}}.")
  }
  if (is.null(states)) {
    lv <- if (is.factor(sequence$state)) levels(sequence$state) else sort(unique(as.character(sequence$state)))
    states <- jn_states(lv)
  } else {
    states <- as_jn_states(states)
  }
  st <- as.character(sequence$state)
  bad <- setdiff(unique(st), states$level)
  if (length(bad) > 0) {
    cli::cli_abort("State{?s} {.val {bad}} {?is/are} not in the states dictionary.")
  }
  sequence$state <- factor(st, levels = states$level)
  sequence$step <- as.integer(sequence$step)
  sequence <- sequence[order(sequence$patient_id, sequence$step), need, drop = FALSE]
  rownames(sequence) <- NULL
  meta$n_persons <- NULL
  tryCatch(
    class_journey(sequence = sequence, states = states, meta = meta),
    error = function(e) cli::cli_abort(conditionMessage(e), call = NULL)
  )
}

#' @rdname journey
#' @format NULL
#' @usage NULL
#' @export
class_journey <- S7::new_class(
  "journey",
  properties = list(
    sequence = S7::class_data.frame,
    states = S7::class_data.frame,
    meta = S7::class_list,
    n_persons = S7::new_property(
      S7::class_integer,
      getter = function(self) length(unique(self@sequence$patient_id))
    )
  ),
  validator = function(self) {
    st <- self@states
    sq <- self@sequence
    if (!all(c("level", "label", "colour") %in% names(st))) {
      return("@states must have columns level, label and colour (see jn_states()).")
    }
    if (anyNA(st$level) || anyDuplicated(st$level)) {
      return("@states$level must be unique and non-missing.")
    }
    miss <- setdiff(jn_sequence_cols, names(sq))
    if (length(miss) > 0) {
      return(paste0("@sequence lacks column(s): ", paste(miss, collapse = ", "), "."))
    }
    if (!is.factor(sq$state) || !identical(levels(sq$state), st$level)) {
      return("@sequence$state must be a factor with the levels of @states$level.")
    }
    if (anyNA(sq$state)) return("@sequence$state must not be missing.")
    if (!is.integer(sq$step)) return("@sequence$step must be an integer.")
    n <- nrow(sq)
    if (n > 0) {
      first <- c(TRUE, sq$patient_id[-1] != sq$patient_id[-n])
      expect <- seq_len(n) - cummax(ifelse(first, seq_len(n), 0L)) + 1L
      if (any(sq$step != expect) || anyDuplicated(sq$patient_id[first])) {
        return("@sequence$step must run 1, 2, 3, ... within each patient (rows sorted by patient and step).")
      }
    }
    if (length(self@meta) > 0 && is.null(names(self@meta))) {
      return("@meta must be a named list.")
    }
    NULL
  }
)

S7::method(print, class_journey) <- function(x, ...) {
  m <- x@meta
  cat(cli::format_inline("<journey> {x@n_persons} patient{?s}, {nrow(x@sequence)} step row{?s}, {nrow(x@states)} state{?s}"), "\n", sep = "")
  used <- table(x@sequence$state)
  lv <- x@states$level
  empty <- lv[used[lv] == 0]
  cat(cli::format_inline("States (node order): {.val {lv}}"), "\n", sep = "")
  if (length(empty) > 0) cat(cli::format_inline("Empty states: {.val {empty}}"), "\n", sep = "")
  if (length(m) > 0) {
    vals <- vapply(m, function(v) {
      if (is.null(v)) "NULL" else paste(format(v), collapse = ", ")
    }, character(1))
    cat(cli::format_inline("Parameters: {paste(names(m), vals, sep = ' = ', collapse = '; ')}"), "\n", sep = "")
  }
  invisible(x)
}

S7::method(summary, class_journey) <- function(object, ...) {
  out <- jn_nodes(object)
  out$pct <- out$n / object@n_persons
  out
}

S7::method(as.data.frame, class_journey) <- function(x, ...) { # nolint: object_name_linter.
  x@sequence
}

S7::method(plot, class_journey) <- function(x, ...) {
  plot_sankey(x, ...)
}

# person x step matrix of integer state codes (NA after a patient's last
# step). Rows follow the order of unique patient ids.
jn_state_matrix <- function(j) {
  s <- j@sequence
  pid <- match(s$patient_id, unique(s$patient_id))
  n_steps <- if (nrow(s) == 0) 0L else max(s$step)
  m <- matrix(NA_integer_, nrow = max(c(0L, pid)), ncol = n_steps)
  m[cbind(pid, s$step)] <- as.integer(s$state)
  m
}

check_journey <- function(j, arg = "j") {
  if (!S7::S7_inherits(j, class_journey)) {
    cli::cli_abort("{.arg {arg}} must be a journey (see {.fn jn_sequence}).")
  }
  invisible(j)
}

# Which steps to use: NULL = all, otherwise sorted unique positive integers
# within the journey.
resolve_steps <- function(steps, n_steps) {
  if (is.null(steps)) return(seq_len(n_steps))
  steps <- sort(unique(as.integer(steps)))
  if (anyNA(steps) || any(steps < 1) || length(steps) == 0) {
    cli::cli_abort("{.arg steps} must be positive integers.")
  }
  steps[steps <= n_steps]
}
