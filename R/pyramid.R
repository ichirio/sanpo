#' Age-sex pyramid
#'
#' * `sp_pyramid_data()` counts patients by age band and sex. The input is
#'   either one row per patient (`age`, `sex`) or an already counted table
#'   (give the count column in `count`). Bands are `width` years wide with an
#'   open top band (`"85+"` by default, as in Japanese claims statistics).
#' * `sp_plot_pyramid()` draws males to the left and females to the right.
#' * `sp_pyramid()` is the two in one call.
#'
#' Sex values are recognised case-insensitively: `"M"`, `"male"`, `"1"` for
#' males and `"F"`, `"female"`, `"2"` for females (the 1 / 2 coding of
#' Japanese claims). Other values are an error; drop or recode them first.
#'
#' @param data Data frame: one row per patient, or counts (see `count`).
#' @param age Age column (numeric years), or, for counted data, an age-band
#'   column used as is.
#' @param sex Sex column.
#' @param count Optional count column for already counted data.
#' @param width Band width in years.
#' @param top Lower limit of the open top band (e.g. 85 for `"85+"`), or
#'   `Inf` for no open band.
#' @return `sp_pyramid_data()`: a data frame with `age_band` (factor), `sex`
#'   (`"M"`, `"F"`), `n` and `pct` (percent of all patients); every band
#'   appears for both sexes, with 0 where empty.
#' @examples
#' claims <- sp_synth_claims(n = 500)
#' patients <- claims[!duplicated(claims$patient_id), ]
#' sp_pyramid(patients)
#' sp_pyramid(patients, top = 75, percent = TRUE)
#'
#' # counted data
#' tab <- data.frame(band = rep(c("0-39", "40-64", "65+"), 2),
#'                   sex = rep(c("M", "F"), each = 3), n = c(10, 25, 30, 12, 22, 41))
#' sp_pyramid(tab, age = "band", count = "n")
#' @export
sp_pyramid_data <- function(data, age = "age", sex = "sex", count = NULL, width = 5, top = 85) {
  sp_check_data(data)
  sp_check_cols(data, list(age = age, sex = sex, count = count))
  sx <- sp_sex(data[[sex]], sex)
  if (is.null(count)) {
    sp_check_numeric_col(data, age, "age", min = 0)
    band <- sp_age_band(data[[age]], width = width, top = top)
    w <- rep(1, nrow(data))
  } else {
    sp_check_numeric_col(data, count, "count", min = 0)
    band <- data[[age]]
    if (anyNA(band)) stop("Column \"", age, "\" (`age`) has missing values.", call. = FALSE)
    if (!is.factor(band)) band <- factor(band, levels = unique(band))
    w <- data[[count]]
  }
  d <- data.frame(age_band = band, sex = factor(sx, levels = c("M", "F")), w = w)
  out <- d |>
    dplyr::group_by(.data$age_band, .data$sex, .drop = FALSE) |>
    dplyr::summarise(n = sum(.data$w), .groups = "drop") |>
    as.data.frame()
  out$sex <- as.character(out$sex)
  out$pct <- 100 * out$n / sum(out$n)
  out
}

#' @rdname sp_pyramid_data
#' @param pyramid A table from `sp_pyramid_data()`.
#' @param percent Show percentages of all patients instead of counts.
#' @param palette Two colours, males then females.
#' @param lang Label language, `"en"` or `"ja"`.
#' @export
sp_plot_pyramid <- function(pyramid, percent = FALSE, palette = sp_palette(2),
                            lang = getOption("sanpo.lang", "en")) {
  lang <- sp_lang(lang)
  sp_check_data(pyramid, "pyramid")
  sp_check_cols(pyramid, list(age_band = "age_band", sex = "sex", n = "n"), "pyramid")
  if (!all(pyramid$sex %in% c("M", "F"))) {
    stop("`pyramid$sex` must be \"M\" or \"F\" (as made by sp_pyramid_data()).", call. = FALSE)
  }
  if (percent && is.null(pyramid$pct)) pyramid$pct <- 100 * pyramid$n / sum(pyramid$n)
  pyramid$value <- if (percent) pyramid$pct else pyramid$n
  pyramid$signed <- ifelse(pyramid$sex == "M", -pyramid$value, pyramid$value)
  sex_lab <- c(M = sp_label("male", lang), F = sp_label("female", lang))
  pyramid$sex_lab <- factor(sex_lab[pyramid$sex], levels = sex_lab)
  if (!is.factor(pyramid$age_band)) {
    pyramid$age_band <- factor(pyramid$age_band, levels = unique(pyramid$age_band))
  }
  band_labels <- levels(pyramid$age_band)
  if (lang == "ja") band_labels <- sp_band_label_ja(band_labels)
  lim <- max(abs(pyramid$signed), 0)

  ggplot2::ggplot(pyramid, ggplot2::aes(x = .data$signed, y = .data$age_band,
                                        fill = .data$sex_lab)) +
    ggplot2::geom_col(width = 0.9, position = "identity") +
    ggplot2::geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.3) +
    ggplot2::scale_x_continuous(labels = function(x) format(abs(x), big.mark = ",", trim = TRUE),
                                limits = c(-lim, lim) * 1.05) +
    ggplot2::scale_y_discrete(labels = band_labels, drop = FALSE) +
    ggplot2::scale_fill_manual(values = stats::setNames(palette[1:2], sex_lab), name = NULL) +
    ggplot2::labs(x = sp_label(if (percent) "percent" else "count", lang),
                  y = sp_label("age_group", lang)) +
    sp_theme(lang = lang) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' @rdname sp_pyramid_data
#' @param ... Further arguments passed to `sp_plot_pyramid()`.
#' @export
sp_pyramid <- function(data, age = "age", sex = "sex", count = NULL, width = 5, top = 85, ...) {
  py <- sp_pyramid_data(data, age = age, sex = sex, count = count, width = width, top = top)
  sp_attach(sp_plot_pyramid(py, ...), py)
}

sp_sex <- function(x, col) {
  if (anyNA(x)) stop("Column \"", col, "\" (`sex`) has missing values.", call. = FALSE)
  s <- tolower(trimws(as.character(x)))
  out <- ifelse(s %in% c("m", "male", "1"), "M", ifelse(s %in% c("f", "female", "2"), "F", NA))
  if (anyNA(out)) {
    stop("Column \"", col, "\" (`sex`) has values that are not male / female: ",
         paste0("\"", utils::head(unique(x[is.na(out)]), 3), "\"", collapse = ", "),
         ". Use M/F, male/female or 1/2.", call. = FALSE)
  }
  out
}

# "0-4" -> "0-4 sai", "85+" -> "85 sai ijou" in Japanese
sp_band_label_ja <- function(x) {
  x <- sub("^([0-9]+)\\+$", "\\1\u6b73\u4ee5\u4e0a", x)
  sub("^([0-9]+)-([0-9]+)$", "\\1\uff5e\\2\u6b73", x)
}
