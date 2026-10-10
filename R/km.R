#' Kaplan-Meier and cumulative incidence curves
#'
#' * `sp_km_data()` fits the curves with `survival::survfit()` and returns
#'   them as tables, with the number at risk at chosen times. With a 0 / 1
#'   status it is a Kaplan-Meier fit. With a status of more than two values
#'   (0 = censored, 1, 2, ... = competing event types; or a factor whose
#'   first level is censoring) it is the Aalen-Johansen estimate of the
#'   cumulative incidence of each event type, which accounts for competing
#'   risks.
#' * `sp_plot_km()` draws the curves (step lines, with confidence bands) and,
#'   optionally, the number at risk as a separate panel below
#'   (`risk_table = TRUE`, needs the 'patchwork' package).
#'   `sp_plot_risk_table()` draws that panel on its own.
#' * `sp_km()` is the two in one call.
#'
#' @param data Data frame with one row per patient.
#' @param time Follow-up time column (numeric, >= 0).
#' @param status Status column: 0 / 1 (or `FALSE` / `TRUE`) for a
#'   Kaplan-Meier fit; 0 / 1 / 2 ... or a factor (first level = censored)
#'   for competing risks.
#' @param group Optional group column (one curve per group).
#' @param times Times at which the number at risk is reported. Default:
#'   about six pretty breaks over the follow-up.
#' @param conf_level Confidence level of the bands.
#' @return `sp_km_data()`: a list of class `sp_km` with `curve` (columns
#'   `time`, `estimate`, `lower`, `upper`, `group`, `state`), `risk` (`time`,
#'   `group`, `n_risk`), `median` (median time per group; Kaplan-Meier
#'   only), `type` (`"km"` or `"cuminc"`) and `fit` (the `survfit` object).
#' @examples
#' surv <- sp_synth_survival(n = 300)
#' sp_km(surv, group = "group")
#' sp_km(surv, group = "group", cumulative = TRUE, risk_table = TRUE)
#'
#' # competing risks: status 1 = event of interest, 2 = death
#' sp_km(surv, status = "status_cr", group = "group", states = "1")
#' @export
sp_km_data <- function(data, time = "time", status = "status", group = NULL, times = NULL,
                       conf_level = 0.95) {
  sp_check_data(data)
  sp_check_cols(data, list(time = time, status = status, group = group))
  sp_check_numeric_col(data, time, "time", min = 0)
  sp_check_number(conf_level, "conf_level", min = 0.5, max = 0.999)
  st <- data[[status]]
  if (anyNA(st)) stop("Column \"", status, "\" (`status`) has missing values.", call. = FALSE)
  if (is.logical(st)) st <- as.integer(st)
  if (is.factor(st)) {
    type <- if (nlevels(st) > 2) "cuminc" else "km"
    if (type == "km") st <- as.integer(st) - 1L
  } else {
    if (!is.numeric(st) || any(st != round(st)) || any(st < 0)) {
      stop("Column \"", status, "\" (`status`) must be 0 / 1 (censored / event), or 0, 1, 2, ... ",
           "for competing risks, or a factor whose first level is censoring.", call. = FALSE)
    }
    type <- if (max(st) > 1) "cuminc" else "km"
    if (type == "cuminc") st <- factor(st, levels = sort(unique(c(0, st))))
  }
  if (type == "km" && !any(st == 1)) {
    warning("No events in column \"", status, "\"; the curve stays at 1.", call. = FALSE)
  }
  g <- if (is.null(group)) factor(rep("all", nrow(data))) else {
    gg <- data[[group]]
    if (anyNA(gg)) stop("Column \"", group, "\" (`group`) has missing values.", call. = FALSE)
    if (is.factor(gg)) droplevels(gg) else factor(gg, levels = sort(unique(gg)))
  }
  d <- data.frame(.time = data[[time]], .status = st, .group = g)
  fit <- if (nlevels(g) > 1) {
    survival::survfit(survival::Surv(.time, .status) ~ .group, data = d, conf.int = conf_level)
  } else {
    survival::survfit(survival::Surv(.time, .status) ~ 1, data = d, conf.int = conf_level)
  }

  strata <- if (is.null(fit$strata)) rep(levels(g), length(fit$time)) else
    rep(sub("^\\.group=", "", names(fit$strata)), fit$strata)
  if (type == "km") {
    curve <- data.frame(time = fit$time, estimate = fit$surv, lower = fit$lower,
                        upper = fit$upper, group = strata, state = "1")
  } else {
    states <- fit$states
    keep <- which(states != "(s0)")
    curve <- do.call(rbind, lapply(keep, function(j) {
      data.frame(time = fit$time, estimate = fit$pstate[, j], lower = fit$lower[, j],
                 upper = fit$upper[, j], group = strata, state = states[j])
    }))
  }
  # start every curve at time 0
  start <- unique(curve[c("group", "state")])
  start$time <- 0
  start$estimate <- start$lower <- start$upper <- if (type == "km") 1 else 0
  curve <- rbind(start[names(curve)], curve)
  curve$group <- factor(curve$group, levels = levels(g))
  curve <- curve[order(curve$group, curve$state, curve$time), , drop = FALSE]
  rownames(curve) <- NULL

  if (is.null(times)) times <- pretty(c(0, max(d$.time)), n = 6)
  times <- times[times <= max(d$.time)]
  risk <- do.call(rbind, lapply(levels(g), function(lv) {
    tt <- d$.time[d$.group == lv]
    data.frame(time = times, group = lv, n_risk = vapply(times, function(t) sum(tt >= t), numeric(1)))
  }))
  risk$group <- factor(risk$group, levels = levels(g))

  med <- NULL
  if (type == "km") {
    med <- do.call(rbind, lapply(levels(g), function(lv) {
      cv <- curve[curve$group == lv, ]
      below <- cv$time[cv$estimate <= 0.5]
      data.frame(group = lv, median = if (length(below)) min(below) else NA_real_)
    }))
  }
  structure(list(curve = curve, risk = risk, median = med, type = type, fit = fit,
                 grouped = !is.null(group)),
            class = "sp_km")
}

#' @export
print.sp_km <- function(x, ...) {
  cat("<sp_km> ", if (x$type == "km") "Kaplan-Meier" else "cumulative incidence (Aalen-Johansen)",
      ", groups: ", paste(levels(x$curve$group), collapse = ", "), "\n", sep = "")
  print(utils::head(x$curve), ...)
  invisible(x)
}

#' @rdname sp_km_data
#' @param km An `sp_km` object from `sp_km_data()`.
#' @param cumulative For a Kaplan-Meier fit, draw 1 - S(t) (the cumulative
#'   incidence) instead of S(t).
#' @param states For competing risks, which event types to draw (default
#'   all). Types are shown by line type.
#' @param ci Draw confidence bands.
#' @param risk_table Add the number-at-risk panel below the curves (needs
#'   'patchwork'; a warning is given and the curves alone are returned
#'   without it).
#' @param time_lab X-axis title. Default `"Time"`; e.g. `"Days"`.
#' @param y_lab Y-axis title. Default from the curve type.
#' @param percent Show the y axis in percent.
#' @param palette Colours for the groups.
#' @param lang Label language, `"en"` or `"ja"`.
#' @return `sp_plot_km()`: a `ggplot`, or a 'patchwork' when `risk_table =
#'   TRUE`.
#' @export
sp_plot_km <- function(km, cumulative = FALSE, states = NULL, ci = TRUE, risk_table = FALSE,
                       time_lab = NULL, y_lab = NULL, percent = TRUE, palette = NULL,
                       lang = getOption("sanpo.lang", "en")) {
  lang <- sp_lang(lang)
  if (!inherits(km, "sp_km")) stop("`km` must be made by sp_km_data().", call. = FALSE)
  cv <- km$curve
  if (km$type == "km" && cumulative) {
    cv$estimate <- 1 - cv$estimate
    lo <- 1 - cv$upper
    cv$upper <- 1 - cv$lower
    cv$lower <- lo
  }
  if (!is.null(states)) {
    bad <- setdiff(as.character(states), unique(cv$state))
    if (length(bad)) {
      stop("`states` not in the fit: ", paste(bad, collapse = ", "), ". Available: ",
           paste(unique(cv$state), collapse = ", "), ".", call. = FALSE)
    }
    cv <- cv[cv$state %in% states, , drop = FALSE]
  }
  n_grp <- nlevels(cv$group)
  pal <- palette %or% sp_palette(n_grp)
  multi_state <- length(unique(cv$state)) > 1
  cv$.line <- interaction(cv$group, cv$state, drop = TRUE)

  p <- ggplot2::ggplot(cv, ggplot2::aes(x = .data$time, y = .data$estimate, group = .data$.line,
                                        colour = .data$group, fill = .data$group))
  if (ci) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                                  alpha = 0.15, colour = NA,
                                  outline.type = "full", na.rm = TRUE,
                                  data = sp_step_ribbon(cv))
  }
  p <- p + if (multi_state) {
    ggplot2::geom_step(ggplot2::aes(linetype = .data$state), linewidth = 0.6)
  } else {
    ggplot2::geom_step(linewidth = 0.6)
  }
  x_lim <- c(0, max(km$curve$time))
  y_default <- if (km$type == "cuminc" || cumulative) "cuminc" else "survival"
  p <- p +
    ggplot2::scale_colour_manual(values = pal, name = NULL,
                                 guide = if (km$grouped) "legend" else "none") +
    ggplot2::scale_fill_manual(values = pal, name = NULL,
                               guide = if (km$grouped) "legend" else "none") +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = ggplot2::expansion(c(0, 0.02)),
                                labels = if (percent) function(x) paste0(100 * x, "%") else ggplot2::waiver()) +
    ggplot2::scale_x_continuous(breaks = unique(km$risk$time), limits = x_lim,
                                expand = ggplot2::expansion(c(0.01, 0.02))) +
    ggplot2::labs(x = time_lab %or% sp_label("time", lang),
                  y = y_lab %or% sp_label(y_default, lang), linetype = NULL) +
    sp_theme(lang = lang)

  if (!risk_table) return(p)
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    warning("The number-at-risk panel needs the 'patchwork' package; returning the curves only.",
            call. = FALSE)
    return(p)
  }
  rt <- sp_plot_risk_table(km, palette = pal, lang = lang)
  n_rows <- nlevels(km$risk$group)
  patchwork::wrap_plots(p, rt, ncol = 1, heights = c(4, 0.6 + 0.35 * n_rows))
}

#' @rdname sp_km_data
#' @export
sp_plot_risk_table <- function(km, palette = NULL, lang = getOption("sanpo.lang", "en")) {
  lang <- sp_lang(lang)
  if (!inherits(km, "sp_km")) stop("`km` must be made by sp_km_data().", call. = FALSE)
  rk <- km$risk
  pal <- palette %or% sp_palette(nlevels(rk$group))
  rk$group <- factor(rk$group, levels = rev(levels(rk$group)))
  ggplot2::ggplot(rk, ggplot2::aes(x = .data$time, y = .data$group, label = .data$n_risk)) +
    ggplot2::geom_text(ggplot2::aes(colour = .data$group), size = 3, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = rev(pal[seq_len(nlevels(rk$group))])) +
    ggplot2::scale_x_continuous(breaks = unique(rk$time), limits = c(0, max(km$curve$time)),
                                expand = ggplot2::expansion(c(0.01, 0.02))) +
    ggplot2::scale_y_discrete(labels = if (km$grouped) ggplot2::waiver() else function(x) "") +
    ggplot2::labs(x = NULL, y = NULL, title = sp_label("number_at_risk", lang)) +
    sp_theme(lang = lang) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = ggplot2::rel(0.9)))
}

#' @rdname sp_km_data
#' @param ... Further arguments passed to `sp_plot_km()`.
#' @export
sp_km <- function(data, time = "time", status = "status", group = NULL, times = NULL,
                  conf_level = 0.95, ...) {
  km <- sp_km_data(data, time = time, status = status, group = group, times = times,
                   conf_level = conf_level)
  sp_attach(sp_plot_km(km, ...), km)
}

# Duplicate points so a ribbon follows the steps of the curve.
sp_step_ribbon <- function(cv) {
  parts <- split(cv, cv$.line, drop = TRUE)
  do.call(rbind, lapply(parts, function(x) {
    x <- x[order(x$time), , drop = FALSE]
    n <- nrow(x)
    if (n < 2) return(x)
    prev <- x[-n, , drop = FALSE]
    prev$time <- x$time[-1]
    out <- rbind(x, prev)
    out[order(out$time, rep(c(1, 0), c(n, n - 1))), , drop = FALSE]
  }))
}
