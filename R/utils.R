# Input checks shared by the data functions. They stop with a message that
# names the argument and the column, so a wrong data shape is caught before
# any computing starts.

sp_check_data <- function(data, arg = "data") {
  if (!is.data.frame(data)) {
    stop("`", arg, "` must be a data frame, not ", class(data)[1], ".", call. = FALSE)
  }
  if (nrow(data) == 0) stop("`", arg, "` has no rows.", call. = FALSE)
  invisible(data)
}

# `cols` is a named character vector: names are the argument names, values
# the column names, e.g. c(time = "claim_ym", id = "patient_id").
sp_check_cols <- function(data, cols, arg = "data") {
  cols <- cols[!vapply(cols, is.null, logical(1))]
  for (nm in names(cols)) {
    v <- cols[[nm]]
    if (!is.character(v) || length(v) != 1 || is.na(v) || !nzchar(v)) {
      stop("`", nm, "` must be a single column name (a string).", call. = FALSE)
    }
  }
  cols <- unlist(cols)
  miss <- !cols %in% names(data)
  if (any(miss)) {
    stop("Column(s) not found in `", arg, "`: ",
         paste0("\"", cols[miss], "\" (`", names(cols)[miss], "`)", collapse = ", "),
         ". Available: ", paste(names(data), collapse = ", "), ".", call. = FALSE)
  }
  invisible(data)
}

sp_check_numeric_col <- function(data, col, arg, allow_na = FALSE, min = -Inf) {
  x <- data[[col]]
  if (!is.numeric(x)) {
    stop("Column \"", col, "\" (`", arg, "`) must be numeric, not ", class(x)[1], ".",
         call. = FALSE)
  }
  if (!allow_na && anyNA(x)) {
    stop("Column \"", col, "\" (`", arg, "`) has missing values.", call. = FALSE)
  }
  if (any(x < min, na.rm = TRUE)) {
    stop("Column \"", col, "\" (`", arg, "`) must be >= ", min, ".", call. = FALSE)
  }
  invisible(x)
}

sp_check_number <- function(x, arg, min = -Inf, max = Inf) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < min || x > max) {
    stop("`", arg, "` must be a single number in [", min, ", ", max, "].", call. = FALSE)
  }
  invisible(x)
}

# Run `expr` with a fixed seed and restore the caller's random state.
sp_with_seed <- function(seed, expr) {
  if (is.null(seed)) return(expr)
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
  expr
}

# Keep the drawn table with the plot, as sp_sankey() does.
sp_attach <- function(p, data) {
  attr(p, "sp_data") <- data
  p
}
