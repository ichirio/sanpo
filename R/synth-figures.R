#' Synthetic example data for the figures
#'
#' Small, entirely made-up data sets for the examples and tests. Nothing in
#' them comes from a real database; the numbers are only plausible.
#'
#' * `sp_synth_claims()`: claims-style records, one row per patient and
#'   claim month (`patient_id`, `sex`, `age`, `pref_code`, `claim_ym` as
#'   `YYYYMM` integer), for [sp_trend()], [sp_pyramid()] and
#'   [sp_pref_tile()]. Ages have two peaks; the number of patients grows
#'   slowly with a dip in spring 2020.
#' * `sp_synth_rates()`: events and person-years by fiscal year and sex,
#'   for [sp_rate()].
#' * `sp_synth_estimates()`: rate ratios with 95% confidence intervals by
#'   subgroup, for [sp_forest()].
#' * `sp_synth_survival()`: one row per patient with follow-up `time` (days),
#'   a 0 / 1 `status`, a competing-risks status `status_cr` (0 censored,
#'   1 event, 2 death) and a `group`, for [sp_km()].
#' * `sp_synth_rx()`: prescription fills (`patient_id`, `drug`,
#'   `fill_date`, `days_supply`), for [sp_episodes()] and
#'   [sp_persistence()].
#' * `sp_synth_pref()`: one made-up rate per prefecture (`pref_code`,
#'   `rate`), for [sp_pref_tile()].
#'
#' @param n Number of patients.
#' @param from,to First and last claim month (`YYYYMM`).
#' @param seed Random seed, or `NULL` to use the current random state. The
#'   global random state is restored on exit when a seed is given.
#' @return A data frame.
#' @examples
#' head(sp_synth_claims(n = 50))
#' sp_synth_rates()
#' sp_synth_estimates()
#' head(sp_synth_survival(n = 50))
#' head(sp_synth_rx(n = 20))
#' head(sp_synth_pref())
#' @name sp_synth
NULL

#' @rdname sp_synth
#' @export
sp_synth_claims <- function(n = 2000, from = 201904L, to = 202403L, seed = 1) {
  sp_check_number(n, "n", min = 1)
  n <- as.integer(n)
  months <- seq(sp_month(from), sp_month(to), by = "month")
  if (length(months) < 1) stop("`to` must not be before `from`.", call. = FALSE)
  sp_with_seed(seed, {
    ids <- sprintf(paste0("P%0", max(4L, nchar(n)), "d"), seq_len(n))
    sex <- sample(c("M", "F"), n, replace = TRUE, prob = c(0.48, 0.52))
    young <- stats::runif(n) < 0.3
    age <- round(ifelse(young, stats::rnorm(n, 35, 10), stats::rnorm(n, 70, 11)))
    age <- pmin(pmax(age, 0), 104)
    pref_w <- stats::runif(47, 0.3, 1) * c(5, rep(1, 9), 6, 5, 12, 8, rep(1, 8), 6, rep(1, 3),
                                         8, 4, rep(1, 11), 4, rep(1, 7))
    pref <- sample(47, n, replace = TRUE, prob = pref_w)
    # monthly probability of a claim: slow growth, a dip in spring 2020
    k <- seq_along(months)
    p_month <- 0.25 * (1 + 0.006 * k)
    covid <- format(months, "%Y-%m") %in% c("2020-04", "2020-05")
    p_month[covid] <- p_month[covid] * 0.7
    first <- sample(length(months), n, replace = TRUE)
    rows <- lapply(seq_len(n), function(i) {
      m <- k[k >= first[i]]
      keep <- m == first[i] | stats::runif(length(m)) < p_month[m]
      m <- m[keep]
      data.frame(patient_id = ids[i], sex = sex[i], age = age[i], pref_code = pref[i],
                 claim_ym = as.integer(format(months[m], "%Y%m")))
    })
    out <- do.call(rbind, rows)
    out <- out[order(out$patient_id, out$claim_ym), , drop = FALSE]
    rownames(out) <- NULL
    out
  })
}

#' @rdname sp_synth
#' @export
sp_synth_rates <- function(seed = 1) {
  sp_with_seed(seed, {
    fy <- 2015:2023
    d <- expand.grid(period = fy, sex = c("M", "F"), stringsAsFactors = FALSE)
    d$person_years <- round(ifelse(d$sex == "M", 410000, 430000) * (1 - 0.004 * (d$period - 2015)))
    base <- ifelse(d$sex == "M", 52, 38) * (1 + 0.035 * (d$period - 2015))
    base[d$period == 2020] <- base[d$period == 2020] * 0.85
    d$events <- stats::rpois(nrow(d), base * d$person_years / 1e5)
    d <- d[order(d$sex, d$period), c("period", "sex", "events", "person_years")]
    rownames(d) <- NULL
    d
  })
}

#' @rdname sp_synth
#' @export
sp_synth_estimates <- function(seed = 1) {
  sp_with_seed(seed, {
    d <- data.frame(
      group = c("Overall", "Sex", "Sex", "Age group", "Age group", "Age group",
                "Diabetes", "Diabetes"),
      label = c("All patients", "Male", "Female", "18-64", "65-74", "75+", "No", "Yes"),
      n = c(4200, 2050, 2150, 1500, 1400, 1300, 3100, 1100)
    )
    log_rr <- stats::rnorm(nrow(d), log(1.3), 0.12)
    log_rr[1] <- log(1.3)
    se <- 1.6 / sqrt(d$n * 0.08)
    d$estimate <- round(exp(log_rr), 3)
    d$lower <- round(exp(log_rr - 1.96 * se), 3)
    d$upper <- round(exp(log_rr + 1.96 * se), 3)
    d
  })
}

#' @rdname sp_synth
#' @export
sp_synth_survival <- function(n = 400, seed = 1) {
  sp_check_number(n, "n", min = 2)
  n <- as.integer(n)
  sp_with_seed(seed, {
    group <- sample(c("Drug A", "Drug B"), n, replace = TRUE)
    rate_event <- ifelse(group == "Drug A", 1 / 900, 1 / 600)
    t_event <- stats::rexp(n, rate_event)
    t_death <- stats::rexp(n, 1 / 2500)
    t_cens <- stats::runif(n, 200, 1095)
    time <- pmin(t_event, t_death, t_cens)
    status_cr <- ifelse(time == t_event, 1L, ifelse(time == t_death, 2L, 0L))
    data.frame(patient_id = sprintf("S%04d", seq_len(n)), group = group,
               time = ceiling(time), status = as.integer(status_cr == 1L),
               status_cr = status_cr)
  })
}

#' @rdname sp_synth
#' @export
sp_synth_rx <- function(n = 300, seed = 1) {
  sp_check_number(n, "n", min = 1)
  n <- as.integer(n)
  sp_with_seed(seed, {
    drug <- sample(c("Drug A", "Drug B"), n, replace = TRUE)
    start <- as.Date("2021-04-01") + sample(0:364, n, replace = TRUE)
    rows <- lapply(seq_len(n), function(i) {
      p_stop <- if (drug[i] == "Drug A") 0.10 else 0.18
      d <- start[i]
      out <- list()
      repeat {
        supply <- sample(c(28L, 30L, 56L, 90L), 1, prob = c(0.4, 0.3, 0.2, 0.1))
        out[[length(out) + 1]] <- data.frame(fill_date = d, days_supply = supply)
        if (stats::runif(1) < p_stop) break
        # usually refilled on time, sometimes late
        late <- if (stats::runif(1) < 0.15) sample(20:60, 1) else sample(-5:10, 1)
        d <- d + supply + late
        if (d > as.Date("2023-03-31")) break
      }
      cbind(patient_id = sprintf("R%04d", i), drug = drug[i], do.call(rbind, out))
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  })
}

#' @rdname sp_synth
#' @export
sp_synth_pref <- function(seed = 1) {
  sp_with_seed(seed, {
    g <- sp_pref_grid_df
    # a made-up east-west gradient plus noise
    data.frame(pref_code = g$code,
               rate = round(80 + 3 * g$col + stats::rnorm(47, 0, 8), 1))
  })
}
