#' Incidence and prevalence with confidence intervals over time
#'
#' * `sp_rate_data()` takes one row per period (and group) with the number of
#'   events (or cases) and the denominator, and adds the rate and its
#'   confidence interval.
#' * `sp_plot_rate()` draws the rates as points with interval bars joined by
#'   lines (one colour per group).
#' * `sp_rate()` is the two in one call.
#'
#' For an incidence rate (`type = "incidence"`, denominator in person-time)
#' the interval is the exact Poisson interval (`ci = "exact"`, the default,
#' from the chi-squared distribution), Byar's approximation (`"byar"`) or the
#' normal approximation on the log scale (`"normal"`). For a prevalence
#' (`type = "prevalence"`, denominator in persons) it is the exact binomial
#' (Clopper-Pearson) interval (`"exact"`) or the normal (Wald) interval
#' (`"normal"`).
#'
#' @param data Data frame with one row per period (and group).
#' @param time Period column (numbers, dates, or labels in time order).
#' @param events Number of events (incidence) or cases (prevalence).
#' @param denom Person-time (incidence) or number of persons (prevalence).
#' @param by Optional grouping column.
#' @param type `"incidence"` or `"prevalence"`.
#' @param ci Interval method: `"exact"`, `"byar"` (incidence only) or
#'   `"normal"`.
#' @param per Multiplier (e.g. `1e5` for per 100,000 person-years, `100`
#'   for percent).
#' @param conf_level Confidence level.
#' @return `sp_rate_data()`: `data` with `rate`, `lower` and `upper` added
#'   (per `per`); attributes record `time`, `by`, `type`, `ci` and `per`.
#' @examples
#' rates <- sp_synth_rates()
#' head(rates)
#' sp_rate(rates, per = 1e5)
#' sp_rate(rates, by = "sex", per = 1e5, ci = "byar")
#' @export
sp_rate_data <- function(data, time = "period", events = "events", denom = "person_years",
                         by = NULL, type = c("incidence", "prevalence"),
                         ci = c("exact", "byar", "normal"), per = 1e5, conf_level = 0.95) {
  type <- match.arg(type)
  ci <- match.arg(ci)
  sp_check_data(data)
  sp_check_cols(data, list(time = time, events = events, denom = denom, by = by))
  sp_check_numeric_col(data, events, "events", min = 0)
  sp_check_numeric_col(data, denom, "denom", min = 0)
  sp_check_number(per, "per", min = .Machine$double.eps)
  sp_check_number(conf_level, "conf_level", min = 0.5, max = 0.999)
  x <- data[[events]]
  n <- data[[denom]]
  if (any(n == 0)) stop("Column \"", denom, "\" (`denom`) has zeros.", call. = FALSE)
  if (any(x != round(x))) stop("Column \"", events, "\" (`events`) must be whole numbers.", call. = FALSE)
  if (type == "prevalence" && any(x > n)) {
    stop("For a prevalence, `events` cannot exceed `denom`.", call. = FALSE)
  }
  if (type == "prevalence" && ci == "byar") {
    stop("`ci = \"byar\"` is for incidence rates; use \"exact\" or \"normal\".", call. = FALSE)
  }
  ci_x <- sp_rate_ci(x, n, type, ci, conf_level)
  out <- data
  out$rate <- x / n * per
  out$lower <- ci_x$lower * per
  out$upper <- ci_x$upper * per
  attr(out, "time") <- time
  attr(out, "by") <- by
  attr(out, "type") <- type
  attr(out, "ci") <- ci
  attr(out, "per") <- per
  attr(out, "conf_level") <- conf_level
  out
}

sp_rate_ci <- function(x, n, type, ci, conf_level) {
  a <- 1 - conf_level
  z <- stats::qnorm(1 - a / 2)
  if (type == "incidence") {
    switch(ci,
      exact = list(lower = ifelse(x == 0, 0, stats::qchisq(a / 2, 2 * x) / 2) / n,
                   upper = stats::qchisq(1 - a / 2, 2 * (x + 1)) / 2 / n),
      byar = list(lower = ifelse(x == 0, 0, x * (1 - 1 / (9 * x) - z / (3 * sqrt(x)))^3) / n,
                  upper = (x + 1) * (1 - 1 / (9 * (x + 1)) + z / (3 * sqrt(x + 1)))^3 / n),
      normal = list(lower = ifelse(x == 0, 0, x * exp(-z / sqrt(x))) / n,
                    upper = ifelse(x == 0, -log(a / 2), x * exp(z / sqrt(x))) / n)
    )
  } else {
    switch(ci,
      exact = list(lower = ifelse(x == 0, 0, stats::qbeta(a / 2, x, n - x + 1)),
                   upper = ifelse(x == n, 1, stats::qbeta(1 - a / 2, x + 1, n - x))),
      normal = {
        p <- x / n
        se <- sqrt(p * (1 - p) / n)
        list(lower = pmax(0, p - z * se), upper = pmin(1, p + z * se))
      }
    )
  }
}

#' @rdname sp_rate_data
#' @param rates A table from `sp_rate_data()`.
#' @param ribbon Draw a shaded band for the interval instead of bars.
#' @param palette Colours for the groups.
#' @param lang Label language, `"en"` or `"ja"`.
#' @export
sp_plot_rate <- function(rates, time = attr(rates, "time") %or% "period",
                         by = attr(rates, "by"), ribbon = FALSE, palette = NULL,
                         lang = getOption("sanpo.lang", "en")) {
  lang <- sp_lang(lang)
  sp_check_data(rates, "rates")
  sp_check_cols(rates, list(time = time, rate = "rate", lower = "lower", upper = "upper", by = by),
                "rates")
  per <- attr(rates, "per") %or% 1
  type <- attr(rates, "type") %or% "incidence"
  per_txt <- format(per, big.mark = ",", scientific = FALSE)
  ylab <- if (lang == "ja") {
    paste0(if (type == "incidence") "\u767a\u751f\u7387" else "\u6709\u75c5\u7387",
           "\uff08", per_txt, if (type == "incidence") "\u4eba\u5e74" else "\u4eba",
           "\u3042\u305f\u308a\u300195% CI\uff09")
  } else {
    paste0(if (type == "incidence") "Incidence rate" else "Prevalence", " per ", per_txt,
           if (type == "incidence") " person-years" else " persons", " (95% CI)")
  }
  if (!is.null(attr(rates, "conf_level")) && attr(rates, "conf_level") != 0.95) {
    ylab <- sub("95%", paste0(100 * attr(rates, "conf_level"), "%"), ylab, fixed = TRUE)
  }
  tm <- rates[[time]]
  discrete <- !(is.numeric(tm) || inherits(tm, "Date"))
  if (discrete && !is.factor(tm)) rates[[time]] <- factor(tm, levels = unique(tm))

  grp <- if (is.null(by)) NULL else rlang::sym(by)
  step <- if (discrete || length(unique(tm)) < 2) 1 else min(diff(sort(unique(as.numeric(tm)))))
  dodge <- if (is.null(by)) "identity" else ggplot2::position_dodge(width = 0.3 * step)
  pal <- palette %or% sp_palette(if (is.null(by)) 1 else length(unique(rates[[by]])))
  p <- ggplot2::ggplot(rates, ggplot2::aes(x = .data[[time]], y = .data$rate,
                                           ymin = .data$lower, ymax = .data$upper,
                                           colour = !!grp, fill = !!grp, group = !!grp %or% 1))
  if (ribbon) {
    p <- p + ggplot2::geom_ribbon(alpha = 0.2, colour = NA)
  } else {
    p <- p + ggplot2::geom_errorbar(width = 0, linewidth = 0.5, position = dodge)
  }
  p <- p + ggplot2::geom_line(linewidth = 0.5, position = dodge) +
    ggplot2::geom_point(size = 1.6, position = dodge)
  if (is.null(by)) {
    p <- p + ggplot2::aes(colour = "all", fill = "all") +
      ggplot2::scale_colour_manual(values = c(all = pal[1]), guide = "none") +
      ggplot2::scale_fill_manual(values = c(all = pal[1]), guide = "none")
  } else {
    p <- p + ggplot2::scale_colour_manual(values = pal, name = NULL) +
      ggplot2::scale_fill_manual(values = pal, name = NULL)
  }
  if (is.numeric(tm) && all(tm == round(tm))) {
    p <- p + ggplot2::scale_x_continuous(breaks = sort(unique(tm)))
  }
  p + ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(c(0, 0.05))) +
    ggplot2::labs(x = NULL, y = ylab) +
    sp_theme(lang = lang)
}

#' @rdname sp_rate_data
#' @param ... Further arguments passed to `sp_plot_rate()`.
#' @export
sp_rate <- function(data, time = "period", events = "events", denom = "person_years", by = NULL,
                    type = c("incidence", "prevalence"), ci = c("exact", "byar", "normal"),
                    per = 1e5, conf_level = 0.95, ...) {
  r <- sp_rate_data(data, time = time, events = events, denom = denom, by = by, type = type,
                    ci = ci, per = per, conf_level = conf_level)
  sp_attach(sp_plot_rate(r, ...), r)
}
