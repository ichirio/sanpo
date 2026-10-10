#' Treatment episodes and persistence
#'
#' * `sp_episodes()` joins prescription fills into continuous treatment
#'   episodes: a fill that starts more than `gap` days after the end of the
#'   supply so far starts a new episode. An episode is *discontinued* when
#'   its supply plus the gap ends before the end of the data (`end_date`);
#'   otherwise the patient is censored at `end_date`.
#' * `sp_persistence_data()` fits a Kaplan-Meier curve of time to
#'   discontinuation (via [sp_km_data()]), censoring at `horizon` days.
#' * `sp_plot_persistence()` draws it ([sp_plot_km()] with persistence
#'   labels and the median in the caption).
#' * `sp_persistence()` is the last two in one call.
#'
#' Supply is not carried over: overlapping fills extend the episode to the
#' latest supply end, they are not added up.
#'
#' @param data Prescription data: one row per fill.
#' @param id Patient identifier column.
#' @param start Fill date column (`Date`).
#' @param days Days-supply column (numeric, > 0).
#' @param gap Allowed gap in days between the end of supply and the next
#'   fill (the grace period).
#' @param end_date End of the data (a `Date`); default the latest supply end
#'   in `data`.
#' @param by Optional column carried to the output (first value per
#'   patient), e.g. the drug or a subgroup.
#' @param first_only Keep only each patient's first episode.
#' @return `sp_episodes()`: a data frame with `id` (named as in `data`),
#'   `episode`, `start`, `end` (last day of supply), `n_fills`, `duration`
#'   (days from the first fill to discontinuation or censoring) and
#'   `discontinued` (1 / 0), plus the `by` column.
#' @examples
#' rx <- sp_synth_rx(n = 300)
#' head(rx)
#' ep <- sp_episodes(rx, gap = 30, by = "drug")
#' head(ep)
#' sp_persistence(ep, group = "drug", risk_table = TRUE)
#'
#' # sensitivity to the gap
#' sp_persistence(sp_episodes(rx, gap = 60, by = "drug"), group = "drug")
#' @export
sp_episodes <- function(data, id = "patient_id", start = "fill_date", days = "days_supply",
                        gap = 30, end_date = NULL, by = NULL, first_only = TRUE) {
  sp_check_data(data)
  sp_check_cols(data, list(id = id, start = start, days = days, by = by))
  if (!inherits(data[[start]], "Date")) {
    stop("Column \"", start, "\" (`start`) must be a Date; convert it with as.Date().",
         call. = FALSE)
  }
  if (anyNA(data[[start]])) stop("Column \"", start, "\" (`start`) has missing values.", call. = FALSE)
  sp_check_numeric_col(data, days, "days", min = 1)
  sp_check_number(gap, "gap", min = 0)
  d <- data.frame(.id = data[[id]], .start = data[[start]], .days = data[[days]])
  if (!is.null(by)) d$.by <- data[[by]]
  d$.end <- d$.start + d$.days - 1
  if (is.null(end_date)) end_date <- max(d$.end)
  end_date <- as.Date(end_date)
  d <- d[order(d$.id, d$.start), , drop = FALSE]

  eps <- lapply(split(d, d$.id, drop = TRUE), function(x) {
    ep <- integer(nrow(x))
    k <- 1L
    cover <- x$.end[1]
    ep[1] <- 1L
    for (i in seq_len(nrow(x))[-1]) {
      if (as.numeric(x$.start[i] - cover) > gap + 1) {
        k <- k + 1L
        cover <- x$.end[i]
      } else {
        cover <- max(cover, x$.end[i])
      }
      ep[i] <- k
    }
    x$.ep <- ep
    out <- do.call(rbind, lapply(split(x, x$.ep), function(e) {
      data.frame(.id = e$.id[1], episode = e$.ep[1], start = min(e$.start),
                 end = max(e$.end), n_fills = nrow(e),
                 .by = if (is.null(by)) NA else e$.by[1])
    }))
    last <- out$episode == max(out$episode)
    out$discontinued <- as.integer(!last | out$end + gap < end_date)
    stop_day <- out$end
    stop_day[out$discontinued == 0] <- end_date
    out$duration <- as.numeric(stop_day - out$start) + 1
    out
  })
  out <- do.call(rbind, eps)
  if (first_only) out <- out[out$episode == 1, , drop = FALSE]
  out <- out[c(".id", "episode", "start", "end", "n_fills", "duration", "discontinued", ".by")]
  names(out)[1] <- id
  if (is.null(by)) out$.by <- NULL else names(out)[names(out) == ".by"] <- by
  rownames(out) <- NULL
  attr(out, "gap") <- gap
  out
}

#' @rdname sp_episodes
#' @param episodes A table from `sp_episodes()` (or any data with a time and
#'   a 0 / 1 discontinuation column).
#' @param time,status Duration and discontinuation columns.
#' @param group Optional group column (one curve per group).
#' @param horizon Follow-up limit in days: longer durations are censored
#'   there.
#' @param times Times for the number at risk. Default: every 90 days.
#' @export
sp_persistence_data <- function(episodes, time = "duration", status = "discontinued",
                                group = NULL, horizon = 365, times = NULL) {
  sp_check_data(episodes, "episodes")
  sp_check_cols(episodes, list(time = time, status = status, group = group), "episodes")
  sp_check_number(horizon, "horizon", min = 1)
  sp_check_numeric_col(episodes, time, "time", min = 0)
  st <- episodes[[status]]
  if (!all(st %in% c(0, 1))) {
    stop("Column \"", status, "\" (`status`) must be 0 / 1 (1 = discontinued).", call. = FALSE)
  }
  d <- episodes
  over <- d[[time]] > horizon
  d[[time]][over] <- horizon
  d[[status]][over] <- 0
  if (is.null(times)) times <- seq(0, horizon, by = if (horizon > 180) 90 else 30)
  km <- sp_km_data(d, time = time, status = status, group = group, times = times)
  km$horizon <- horizon
  km
}

#' @rdname sp_episodes
#' @param km An `sp_km` object from `sp_persistence_data()`.
#' @param show_median Give the median persistence in the caption.
#' @param lang Label language, `"en"` or `"ja"`.
#' @param ... Further arguments passed to [sp_plot_km()] (e.g.
#'   `risk_table`, `ci`, `palette`).
#' @export
sp_plot_persistence <- function(km, show_median = TRUE, lang = getOption("sanpo.lang", "en"),
                                ...) {
  lang <- sp_lang(lang)
  if (!inherits(km, "sp_km") || km$type != "km") {
    stop("`km` must be made by sp_persistence_data().", call. = FALSE)
  }
  p <- sp_plot_km(km, y_lab = sp_label("persistence", lang),
                  time_lab = sp_label("days", lang), lang = lang, ...)
  if (show_median && !is.null(km$median)) {
    md <- km$median
    txt <- ifelse(is.na(md$median), paste0("> ", km$horizon %or% max(km$curve$time)),
                  format(md$median))
    unit <- if (lang == "ja") "\u65e5" else " days"
    cap <- paste0(sp_label("median", lang), ": ",
                  if (km$grouped) paste0(md$group, " ", txt, unit, collapse = "; ") else
                    paste0(txt, unit))
    p <- p + ggplot2::labs(caption = cap)
  }
  p
}

#' @rdname sp_episodes
#' @export
sp_persistence <- function(episodes, time = "duration", status = "discontinued", group = NULL,
                           horizon = 365, times = NULL, ...) {
  km <- sp_persistence_data(episodes, time = time, status = status, group = group,
                            horizon = horizon, times = times)
  sp_attach(sp_plot_persistence(km, ...), km)
}
