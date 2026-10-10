#' Theme, palette and labels for sanpo figures
#'
#' `sp_theme()` is the theme every sanpo plot uses: `theme_minimal()` at a
#' 10 pt base size, light grey grid lines and the legend at the bottom, which
#' reads well in print. Add it to any `ggplot`, or override it afterwards with
#' `+ ggplot2::theme()`.
#'
#' `sp_palette()` returns colours: Okabe-Ito (the default; distinguishable
#' with the common colour-vision deficiencies), a grey scale for black-and-
#' white print, or viridis.
#'
#' `sp_label()` looks up the axis and legend words used by the plot functions
#' (`"patients"`, `"fiscal_year"`, `"male"`, ...) in English (`lang = "en"`,
#' the default) or Japanese (`lang = "ja"`).
#'
#' @param base_size Base font size in points.
#' @param base_family Base font family. With `lang = "ja"` and the default
#'   `NULL`, a Japanese font is looked up with the 'systemfonts' package when
#'   it is installed (Noto Sans CJK JP, Noto Sans JP, BIZ UDPGothic,
#'   IPAexGothic, Yu Gothic, Hiragino Sans, Meiryo); otherwise the device
#'   default is used.
#' @param lang Label language: `"en"` (default) or `"ja"`. The default can be
#'   set with `options(sanpo.lang = "ja")`.
#' @param legend_position Legend position, passed to `ggplot2::theme()`.
#' @return `sp_theme()`: a `ggplot2` theme. `sp_palette()`: a character
#'   vector of colours. `sp_label()`: a character vector.
#' @examples
#' sp_palette(3)
#' sp_palette(4, "grey")
#' sp_label(c("patients", "fiscal_year"))
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + sp_theme()
#' @export
sp_theme <- function(base_size = 10, base_family = NULL, lang = getOption("sanpo.lang", "en"),
                     legend_position = "bottom") {
  lang <- sp_lang(lang)
  if (is.null(base_family)) base_family <- if (lang == "ja") sp_font_ja() else ""
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "grey40", linewidth = 0.3),
      axis.ticks = ggplot2::element_line(colour = "grey40", linewidth = 0.3),
      legend.position = legend_position,
      plot.title.position = "plot",
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
}

#' @rdname sp_theme
#' @param n Number of colours.
#' @param name Palette: `"okabe_ito"`, `"grey"` or `"viridis"`.
#' @export
sp_palette <- function(n = 8, name = c("okabe_ito", "grey", "viridis")) {
  name <- match.arg(name)
  sp_check_number(n, "n", min = 1)
  n <- as.integer(n)
  switch(name,
    okabe_ito = {
      # Okabe-Ito without black, which is kept for reference lines
      pal <- c("#0072B2", "#D55E00", "#009E73", "#E69F00", "#56B4E9", "#CC79A7",
               "#F0E442", "#999999")
      if (n > length(pal)) {
        warning("Okabe-Ito has ", length(pal), " colours; recycling for n = ", n, ".",
                call. = FALSE)
      }
      rep(pal, length.out = n)
    },
    grey = if (n == 1) "grey30" else grDevices::grey.colors(n, start = 0.15, end = 0.75),
    viridis = grDevices::hcl.colors(n, "viridis")
  )
}

#' @rdname sp_theme
#' @param key Label key(s); see the examples. Unknown keys are returned as is.
#' @export
sp_label <- function(key, lang = getOption("sanpo.lang", "en")) {
  lang <- sp_lang(lang)
  dict <- sp_labels_dict[[lang]]
  out <- unname(dict[key])
  out[is.na(out)] <- key[is.na(out)]
  out
}

sp_lang <- function(lang) {
  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("en", "ja")) {
    stop("`lang` must be \"en\" or \"ja\".", call. = FALSE)
  }
  lang
}

sp_font_ja <- function() {
  if (!requireNamespace("systemfonts", quietly = TRUE)) return("")
  cand <- c("Noto Sans CJK JP", "Noto Sans JP", "BIZ UDPGothic", "IPAexGothic", "Yu Gothic",
            "Hiragino Sans", "Meiryo")
  have <- unique(systemfonts::system_fonts()$family)
  hit <- cand[cand %in% have]
  if (length(hit) == 0) {
    warning("No Japanese font found; using the default font. Japanese labels may not ",
            "display. Install e.g. Noto Sans CJK JP, or set `base_family`.", call. = FALSE)
    return("")
  }
  hit[1]
}

sp_labels_dict <- list(
  en = c(
    patients = "Patients", records = "Records", month = "Month", fiscal_year = "Fiscal year",
    year = "Year", male = "Male", female = "Female", age_group = "Age group",
    count = "Number of patients", percent = "Percent of patients",
    rate = "Rate", per = "per", time = "Time", days = "Days", months = "Months",
    years = "Years", survival = "Survival probability",
    cuminc = "Cumulative incidence", persistence = "Persistent on treatment",
    number_at_risk = "Number at risk", estimate = "Estimate (95% CI)",
    prefecture = "Prefecture", value = "Value", all = "All", fy_prefix = "FY",
    fy_suffix = "", over = "+", median = "Median"
  ),
  ja = c(
    patients = "\u60a3\u8005\u6570", records = "\u30ec\u30b3\u30fc\u30c9\u6570", month = "\u5e74\u6708", fiscal_year = "\u5e74\u5ea6",
    year = "\u5e74", male = "\u7537\u6027", female = "\u5973\u6027", age_group = "\u5e74\u9f62\u968e\u7d1a",
    count = "\u60a3\u8005\u6570", percent = "\u60a3\u8005\u306e\u5272\u5408\uff08%\uff09",
    rate = "\u7387", per = "\u4eba\u3042\u305f\u308a", time = "\u6642\u9593", days = "\u65e5", months = "\u6708",
    years = "\u5e74", survival = "\u751f\u5b58\u78ba\u7387",
    cuminc = "\u7d2f\u7a4d\u767a\u751f\u5272\u5408", persistence = "\u6cbb\u7642\u7d99\u7d9a\u5272\u5408",
    number_at_risk = "\u30ea\u30b9\u30af\u96c6\u5408\u306e\u4eba\u6570", estimate = "\u63a8\u5b9a\u5024\uff0895% CI\uff09",
    prefecture = "\u90fd\u9053\u5e9c\u770c", value = "\u5024", all = "\u5168\u4f53", fy_prefix = "",
    fy_suffix = "\u5e74\u5ea6", over = "\u6b73\u4ee5\u4e0a", median = "\u4e2d\u592e\u5024"
  )
)
