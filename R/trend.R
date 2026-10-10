#' Counts and rates over time
#'
#' Monthly, fiscal-year or calendar-year trends from claims-style data (one
#' row per record, e.g. per patient and claim month).
#'
#' * `sp_trend_data()` counts distinct patients (or records) per period and,
#'   optionally, per group; periods with no records inside the observed range
#'   are kept as 0. Given `denominator`, it also computes a rate.
#' * `sp_plot_trend()` draws the table as lines and points. Monthly plots get
#'   faint vertical lines at the start of each fiscal year.
#' * `sp_trend()` is the two in one call:
#'   `sp_plot_trend(sp_trend_data(data, ...))`.
#'
#' Japanese fiscal years start in April (`fiscal_start = 4`), so
#' `unit = "fiscal_year"` puts April 2019 to March 2020 in fiscal year 2019.
#'
#' @param data Data frame with one row per record.
#' @param id Patient identifier column.
#' @param time Claim month column: `Date` or `YYYYMM` (number or string).
#' @param unit `"month"`, `"fiscal_year"` or `"year"`.
#' @param measure `"patients"` (distinct `id` per period) or `"records"`
#'   (rows per period).
#' @param by Optional grouping column (e.g. `"sex"`).
#' @param fiscal_start First month of the fiscal year (4 = April).
#' @param denominator Optional data frame of denominators with a `period`
#'   column (a `Date` for months, an integer year otherwise), the `by`
#'   column when `by` is given, and a numeric `denom` column (population or
#'   person-time). The rate is `n / denom * per`.
#' @param per Multiplier of the rate (e.g. `1e5` for per 100,000).
#' @return `sp_trend_data()`: a data frame with `period`, `label` (display
#'   label of the period), the `by` column if given, `n` and `value` (equal
#'   to `n`, or the rate when `denominator` is given, then with `denom`).
#'   Attributes `unit`, `measure`, `by` and `per` describe it.
#' @seealso [sp_rate()] for rates with confidence intervals.
#' @examples
#' claims <- sp_synth_claims(n = 300)
#' sp_trend(claims)
#' sp_trend(claims, unit = "fiscal_year", by = "sex")
#'
#' # the same in two steps
#' tr <- sp_trend_data(claims, unit = "fiscal_year", by = "sex")
#' sp_plot_trend(tr)
#' @export
sp_trend_data <- function(data, id = "patient_id", time = "claim_ym",
                          unit = c("month", "fiscal_year", "year"),
                          measure = c("patients", "records"), by = NULL, fiscal_start = 4,
                          denominator = NULL, per = 1) {
  unit <- match.arg(unit)
  measure <- match.arg(measure)
  sp_check_data(data)
  sp_check_cols(data, list(id = id, time = time, by = by))
  sp_check_number(per, "per", min = .Machine$double.eps)
  month <- sp_month(data[[time]])
  if (anyNA(month)) stop("Column \"", time, "\" (`time`) has missing values.", call. = FALSE)

  d <- data.frame(.id = data[[id]], .month = month)
  d$period <- switch(unit,
    month = month,
    fiscal_year = sp_fiscal_year(month, start = fiscal_start),
    year = as.integer(format(month, "%Y"))
  )
  if (!is.null(by)) d[[by]] <- data[[by]]
  grp <- c("period", by)

  out <- d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
    dplyr::summarise(n = if (measure == "patients") dplyr::n_distinct(.data$.id) else dplyr::n(),
                     .groups = "drop")
  # keep empty periods inside the observed range as 0
  all_periods <- if (unit == "month") {
    seq(min(d$period), max(d$period), by = "month")
  } else {
    seq.int(min(d$period), max(d$period))
  }
  full <- if (is.null(by)) {
    data.frame(period = all_periods)
  } else {
    tidyr::expand_grid(period = all_periods, !!by := unique(d[[by]]))
  }
  out <- dplyr::left_join(full, out, by = grp) |>
    dplyr::mutate(n = dplyr::coalesce(.data$n, 0L)) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(grp)))

  out$value <- as.numeric(out$n)
  if (!is.null(denominator)) {
    sp_check_data(denominator, "denominator")
    sp_check_cols(denominator, list(period = "period", by = by, denom = "denom"), "denominator")
    sp_check_numeric_col(denominator, "denom", "denominator$denom", min = 0)
    den <- denominator[c(grp, "denom")]
    if (unit == "month") den$period <- sp_month(den$period)
    if (anyDuplicated(den[grp])) {
      stop("`denominator` has more than one row per period", if (!is.null(by)) " and group",
           ".", call. = FALSE)
    }
    out <- dplyr::left_join(out, den, by = grp)
    if (anyNA(out$denom)) {
      stop("`denominator` has no `denom` for some periods: ",
           paste(utils::head(unique(format(out$period[is.na(out$denom)])), 3), collapse = ", "),
           ".", call. = FALSE)
    }
    out$value <- out$n / out$denom * per
  }
  out$label <- sp_period_label(out$period, unit)
  out <- as.data.frame(out[c("period", "label", by, "n", intersect("denom", names(out)), "value")])
  attr(out, "unit") <- unit
  attr(out, "measure") <- measure
  attr(out, "by") <- by
  attr(out, "per") <- if (is.null(denominator)) NULL else per
  attr(out, "fiscal_start") <- fiscal_start
  out
}

#' @rdname sp_trend_data
#' @param trend A table from `sp_trend_data()` (or any data frame with
#'   `period` and `value`, plus the `by` column).
#' @param fy_lines Draw faint vertical lines at the start of each fiscal year
#'   on monthly plots.
#' @param point Draw points as well as lines.
#' @param palette Colours for the groups (default `sp_palette()`).
#' @param lang Label language, `"en"` or `"ja"`.
#' @export
sp_plot_trend <- function(trend, by = attr(trend, "by"), fy_lines = TRUE, point = TRUE,
                          palette = NULL, lang = getOption("sanpo.lang", "en")) {
  lang <- sp_lang(lang)
  sp_check_data(trend, "trend")
  sp_check_cols(trend, list(period = "period", value = "value", by = by), "trend")
  unit <- attr(trend, "unit") %or% if (inherits(trend$period, "Date")) "month" else "year"
  measure <- attr(trend, "measure") %or% "patients"
  per <- attr(trend, "per")
  fiscal_start <- attr(trend, "fiscal_start") %or% 4

  ylab <- sp_label(measure, lang)
  if (!is.null(per)) {
    ylab <- if (lang == "ja") {
      paste0(ylab, "\uff08", format(per, big.mark = ",", scientific = FALSE), sp_label("per", lang), "\uff09")
    } else {
      paste0(sp_label("rate", lang), " ", sp_label("per", lang), " ",
             format(per, big.mark = ",", scientific = FALSE))
    }
  }
  xlab <- sp_label(unit, lang)

  aes_col <- if (is.null(by)) NULL else rlang::sym(by)
  p <- ggplot2::ggplot(trend, ggplot2::aes(x = .data$period, y = .data$value,
                                           colour = !!aes_col, group = !!aes_col))
  if (unit == "month" && fy_lines) {
    rng <- range(trend$period)
    starts <- seq(as.Date(paste0(as.integer(format(rng[1], "%Y")) - 1, "-",
                                 sprintf("%02d", fiscal_start), "-01")), rng[2], by = "year")
    starts <- starts[starts > rng[1]]
    if (length(starts)) {
      p <- p + ggplot2::geom_vline(xintercept = starts, colour = "grey80", linewidth = 0.3)
    }
  }
  if (is.null(by)) {
    col <- (palette %or% sp_palette(1))[1]
    p <- p + ggplot2::geom_line(linewidth = 0.6, colour = col)
    if (point) p <- p + ggplot2::geom_point(size = 1.2, colour = col)
  } else {
    p <- p + ggplot2::geom_line(linewidth = 0.6)
    if (point) p <- p + ggplot2::geom_point(size = 1.2)
    n_grp <- length(unique(trend[[by]]))
    p <- p + ggplot2::scale_colour_manual(values = palette %or% sp_palette(n_grp), name = NULL)
  }
  if (unit == "month") {
    p <- p + ggplot2::scale_x_date(date_labels = "%Y-%m")
  } else {
    p <- p + ggplot2::scale_x_continuous(breaks = unique(trend$period),
                                         labels = sp_period_label(unique(trend$period), unit, lang))
  }
  p + ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(c(0, 0.05))) +
    ggplot2::labs(x = xlab, y = ylab) +
    sp_theme(lang = lang)
}

#' @rdname sp_trend_data
#' @param ... Further arguments passed to `sp_plot_trend()`.
#' @export
sp_trend <- function(data, id = "patient_id", time = "claim_ym",
                     unit = c("month", "fiscal_year", "year"),
                     measure = c("patients", "records"), by = NULL, fiscal_start = 4,
                     denominator = NULL, per = 1, ...) {
  tr <- sp_trend_data(data, id = id, time = time, unit = unit, measure = measure, by = by,
                      fiscal_start = fiscal_start, denominator = denominator, per = per)
  sp_attach(sp_plot_trend(tr, ...), tr)
}

sp_period_label <- function(period, unit, lang = "en") {
  switch(unit,
    month = format(period, "%Y-%m"),
    fiscal_year = paste0(sp_label("fy_prefix", lang), period, sp_label("fy_suffix", lang)),
    year = as.character(period)
  )
}
