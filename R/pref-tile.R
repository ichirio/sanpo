#' Prefecture tile-grid map
#'
#' A map of Japan's 47 prefectures as equal tiles on a grid, roughly in
#' their geographic places (Hokkaido top right, Okinawa bottom left). It
#' needs no boundary data, every prefecture gets the same area whatever its
#' size, and it reads well in black and white.
#'
#' * `sp_pref_grid()` returns the built-in grid: `code` (JIS X 0401, 1 to
#'   47), English and Japanese names, a unique three-letter English
#'   abbreviation (`abbr`, used for the tile labels) and the tile position (`col`, `row`;
#'   row 0 is the top). The layout was made for this package.
#' * `sp_pref_tile_data()` matches a value per prefecture to the grid.
#'   Prefectures are recognised by code (`1`, `"01"`, `13`) or by English or
#'   Japanese name (`"Tokyo"`, with or without the to/do/fu/ken suffix in
#'   Japanese). Prefectures missing from `data` are kept with `NA`.
#' * `sp_plot_pref_tile()` draws the tiles filled by value.
#' * `sp_pref_tile()` is the two in one call.
#'
#' @return `sp_pref_grid()` and `sp_pref_tile_data()`: a data frame with one
#'   row per prefecture (`code`, `name_en`, `name_ja`, `abbr`, `col`, `row`, and
#'   `value` for the latter).
#' @examples
#' head(sp_pref_grid())
#' pref <- sp_synth_pref()
#' sp_pref_tile(pref, value = "rate", legend_title = "Per 100,000")
#' sp_pref_tile(pref, value = "rate", label = "code", palette = "grey")
#' @export
sp_pref_grid <- function() {
  sp_pref_grid_df
}

#' @rdname sp_pref_grid
#' @param data Data frame with one row per prefecture.
#' @param pref Prefecture column: codes (1 to 47) or names.
#' @param value Value column (numeric).
#' @export
sp_pref_tile_data <- function(data, pref = "pref_code", value = "value") {
  sp_check_data(data)
  sp_check_cols(data, list(pref = pref, value = value))
  sp_check_numeric_col(data, value, "value", allow_na = TRUE)
  code <- sp_pref_code(data[[pref]], pref)
  if (anyDuplicated(code)) {
    stop("Column \"", pref, "\" (`pref`) has more than one row for: ",
         paste(sp_pref_grid_df$name_en[unique(code[duplicated(code)])], collapse = ", "),
         ". Give one value per prefecture.", call. = FALSE)
  }
  out <- sp_pref_grid_df
  out$value <- data[[value]][match(out$code, code)]
  out
}

#' @rdname sp_pref_grid
#' @param tiles A table from `sp_pref_tile_data()`.
#' @param label Tile text: `"name"` (in the label language), `"code"` or
#'   `"none"`.
#' @param palette Fill scale: `"viridis"`, `"grey"` or `"blues"`.
#' @param legend_title Legend title.
#' @param lang Label language, `"en"` or `"ja"`.
#' @export
sp_plot_pref_tile <- function(tiles, label = c("name", "code", "none"),
                              palette = c("viridis", "grey", "blues"), legend_title = NULL,
                              lang = getOption("sanpo.lang", "en")) {
  label <- match.arg(label)
  palette <- match.arg(palette)
  lang <- sp_lang(lang)
  sp_check_data(tiles, "tiles")
  sp_check_cols(tiles, list(col = "col", row = "row", value = "value", code = "code"), "tiles")
  nm <- if (lang == "ja") tiles$name_ja else tiles$name_en
  if (lang == "en") nm <- tiles$abbr %or% nm
  tiles$.label <- switch(label, name = nm, code = sprintf("%02d", tiles$code), none = "")
  rng <- range(tiles$value, na.rm = TRUE)
  mid <- if (all(is.finite(rng))) mean(rng) else 0
  fill_scale <- switch(palette,
    viridis = ggplot2::scale_fill_viridis_c(na.value = "grey85", name = legend_title),
    grey = ggplot2::scale_fill_gradient(low = "grey95", high = "grey20", na.value = "white",
                                        name = legend_title),
    blues = ggplot2::scale_fill_gradient(low = "#DEEBF7", high = "#08519C", na.value = "grey85",
                                         name = legend_title)
  )
  dark_high <- palette != "viridis"
  tiles$.dark <- if (dark_high) tiles$value > mid else tiles$value < mid
  tiles$.dark[is.na(tiles$.dark)] <- FALSE
  ggplot2::ggplot(tiles, ggplot2::aes(x = .data$col, y = -.data$row)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$value), colour = "white", linewidth = 0.6,
                       width = 0.95, height = 0.95) +
    ggplot2::geom_text(ggplot2::aes(label = .data$.label,
                                    colour = .data$.dark), size = 2.4, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey10")) +
    fill_scale +
    ggplot2::coord_equal() +
    sp_theme(lang = lang, legend_position = "right") +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank())
}

#' @rdname sp_pref_grid
#' @param ... Further arguments passed to `sp_plot_pref_tile()`.
#' @export
sp_pref_tile <- function(data, pref = "pref_code", value = "value", ...) {
  tl <- sp_pref_tile_data(data, pref = pref, value = value)
  sp_attach(sp_plot_pref_tile(tl, ...), tl)
}

sp_pref_code <- function(x, col) {
  if (anyNA(x)) stop("Column \"", col, "\" (`pref`) has missing values.", call. = FALSE)
  g <- sp_pref_grid_df
  s <- trimws(as.character(x))
  code <- suppressWarnings(as.integer(ifelse(grepl("^[0-9]+$", s), s, NA)))
  by_en <- match(tolower(sub("(-|\\s)(to|do|fu|ken|prefecture)$", "", s, ignore.case = TRUE)),
                 tolower(g$name_en))
  by_ja <- match(s, g$name_ja)
  by_ja[is.na(by_ja)] <- match(sub("[\u90fd\u5e9c\u770c]$", "", s[is.na(by_ja)]), g$name_ja)
  code <- ifelse(is.na(code), ifelse(is.na(by_en), by_ja, by_en), code)
  bad <- is.na(code) | code < 1 | code > 47
  if (any(bad)) {
    stop("Column \"", col, "\" (`pref`) has values that are not prefectures: ",
         paste0("\"", utils::head(unique(x[bad]), 3), "\"", collapse = ", "),
         ". Use codes 1 to 47 or prefecture names.", call. = FALSE)
  }
  code
}

# Tile layout of the 47 prefectures, made for this package (no third-party
# data). Japanese names are stored without the to/do/fu/ken suffix.
sp_pref_grid_df <- data.frame(
  code = 1:47,
  name_en = c(
    "Hokkaido", "Aomori", "Iwate", "Miyagi", "Akita", "Yamagata", "Fukushima",
    "Ibaraki", "Tochigi", "Gunma", "Saitama", "Chiba", "Tokyo", "Kanagawa", "Niigata",
    "Toyama", "Ishikawa", "Fukui", "Yamanashi", "Nagano", "Gifu", "Shizuoka", "Aichi",
    "Mie", "Shiga", "Kyoto", "Osaka", "Hyogo", "Nara", "Wakayama", "Tottori", "Shimane",
    "Okayama", "Hiroshima", "Yamaguchi", "Tokushima", "Kagawa", "Ehime", "Kochi",
    "Fukuoka", "Saga", "Nagasaki", "Kumamoto", "Oita", "Miyazaki", "Kagoshima",
    "Okinawa"),
  name_ja = c(
    "\u5317\u6d77\u9053", "\u9752\u68ee", "\u5ca9\u624b", "\u5bae\u57ce",
    "\u79cb\u7530", "\u5c71\u5f62", "\u798f\u5cf6", "\u8328\u57ce", "\u6803\u6728",
    "\u7fa4\u99ac", "\u57fc\u7389", "\u5343\u8449", "\u6771\u4eac",
    "\u795e\u5948\u5ddd", "\u65b0\u6f5f", "\u5bcc\u5c71", "\u77f3\u5ddd",
    "\u798f\u4e95", "\u5c71\u68a8", "\u9577\u91ce", "\u5c90\u961c", "\u9759\u5ca1",
    "\u611b\u77e5", "\u4e09\u91cd", "\u6ecb\u8cc0", "\u4eac\u90fd", "\u5927\u962a",
    "\u5175\u5eab", "\u5948\u826f", "\u548c\u6b4c\u5c71", "\u9ce5\u53d6",
    "\u5cf6\u6839", "\u5ca1\u5c71", "\u5e83\u5cf6", "\u5c71\u53e3", "\u5fb3\u5cf6",
    "\u9999\u5ddd", "\u611b\u5a9b", "\u9ad8\u77e5", "\u798f\u5ca1", "\u4f50\u8cc0",
    "\u9577\u5d0e", "\u718a\u672c", "\u5927\u5206", "\u5bae\u5d0e",
    "\u9e7f\u5150\u5cf6", "\u6c96\u7e04"),
  col = c(
    13, 13, 13, 13, 12, 12, 13, 14, 13, 12, 12, 13, 12, 12, 11, 10, 9, 9, 11, 11, 10,
    11, 10, 10, 9, 8, 8, 7, 9, 8, 6, 5, 6, 5, 4, 7, 6, 5, 6, 3, 2, 1, 2, 3, 3, 2, 0),
  row = c(
    0, 1, 2, 3, 2, 3, 4, 5, 5, 4, 5, 6, 6, 7, 3, 3, 3, 4, 5, 4, 4, 6, 5, 6, 5, 5, 6, 5,
    6, 7, 5, 5, 6, 6, 6, 7, 7, 7, 8, 6, 6, 6, 7, 7, 8, 8, 9),
  stringsAsFactors = FALSE
)
sp_pref_grid_df$abbr <- toupper(substr(sp_pref_grid_df$name_en, 1, 3))
local({
  fix <- c(Fukushima = "FKS", Fukui = "FKI", Fukuoka = "FKO", Miyagi = "MYG", Miyazaki = "MYZ",
           Kagawa = "KGW", Kagoshima = "KGS", Tokushima = "TKS", Shiga = "SIG", Shimane = "SMN",
           Shizuoka = "SZO", Yamagata = "YGT", Yamanashi = "YNS", Yamaguchi = "YGC",
           Nagano = "NGN", Nagasaki = "NGS")
  i <- match(names(fix), sp_pref_grid_df$name_en)
  sp_pref_grid_df$abbr[i] <<- unname(fix)
})
sp_pref_grid_df <- sp_pref_grid_df[c("code", "name_en", "name_ja", "abbr", "col", "row")]
