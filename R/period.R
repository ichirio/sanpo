#' Fiscal years, months and age bands
#'
#' Small helpers used by the data functions, exported because they are handy
#' on their own.
#'
#' `sp_month()` turns a claim month into the first day of that month (a
#' `Date`). It accepts `Date`/`POSIXct` values or `YYYYMM` numbers / strings
#' (`201904`, `"2019-04"`).
#'
#' `sp_fiscal_year()` gives the fiscal year a month belongs to. Japanese
#' fiscal years start in April (`start = 4`): April 2019 to March 2020 is
#' fiscal year 2019.
#'
#' `sp_age_band()` cuts ages into bands of `width` years with an open top
#' band (`"85+"` by default, as in Japanese claims statistics).
#'
#' @param x Dates, or claim months as `YYYYMM` numbers or strings.
#' @param start First month of the fiscal year (1 to 12; 4 = April).
#' @return `sp_month()`: a `Date`. `sp_fiscal_year()`: an integer.
#'   `sp_age_band()`: a factor ordered from the youngest band.
#' @examples
#' sp_month(c(201903, 201904))
#' sp_fiscal_year(c(201903, 201904))
#' sp_fiscal_year(as.Date("2020-02-15"))
#' sp_age_band(c(3, 47, 85, 101))
#' sp_age_band(c(3, 47, 85, 101), width = 10, top = 100)
#' @export
sp_month <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) {
    x <- as.Date(x)
    return(as.Date(format(x, "%Y-%m-01")))
  }
  s <- gsub("[^0-9]", "", as.character(x))
  bad <- !is.na(x) & (nchar(s) != 6 | is.na(suppressWarnings(as.integer(substr(s, 5, 6)))))
  mm <- suppressWarnings(as.integer(substr(s, 5, 6)))
  bad <- bad | (!is.na(mm) & (mm < 1 | mm > 12))
  if (any(bad)) {
    stop("Claim months must be dates or YYYYMM values; not understood: ",
         paste(utils::head(unique(x[bad]), 3), collapse = ", "), ".", call. = FALSE)
  }
  out <- as.Date(ifelse(is.na(x), NA_character_,
                        paste0(substr(s, 1, 4), "-", substr(s, 5, 6), "-01")))
  out
}

#' @rdname sp_month
#' @export
sp_fiscal_year <- function(x, start = 4) {
  sp_check_number(start, "start", min = 1, max = 12)
  m <- sp_month(x)
  y <- as.integer(format(m, "%Y"))
  mo <- as.integer(format(m, "%m"))
  ifelse(mo >= start, y, y - 1L)
}

#' @rdname sp_month
#' @param age Ages in years (numeric, >= 0).
#' @param width Band width in years.
#' @param top Lower limit of the open top band (must be a multiple of
#'   `width`), or `Inf` for no open band.
#' @export
sp_age_band <- function(age, width = 5, top = 85) {
  if (!is.numeric(age)) stop("`age` must be numeric.", call. = FALSE)
  if (any(age < 0, na.rm = TRUE)) stop("`age` must be >= 0.", call. = FALSE)
  sp_check_number(width, "width", min = 1)
  if (!is.numeric(top) || length(top) != 1 || is.na(top) || top <= 0) {
    stop("`top` must be a single positive number or Inf.", call. = FALSE)
  }
  if (is.finite(top) && top %% width != 0) {
    stop("`top` (", top, ") must be a multiple of `width` (", width, ").", call. = FALSE)
  }
  hi <- if (is.finite(top)) top else max(width, ceiling((max(age, na.rm = TRUE) + 1) / width) * width)
  lower <- seq(0, hi - width, by = width)
  labs <- paste0(lower, "-", lower + width - 1)
  breaks <- c(lower, hi)
  if (is.finite(top)) {
    labs <- c(labs, paste0(top, "+"))
    breaks <- c(breaks, Inf)
  }
  cut(age, breaks = breaks, labels = labs, right = FALSE)
}
