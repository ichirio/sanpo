#' Save a figure for a journal
#'
#' Write a plot as PNG, TIFF (LZW-compressed) or SVG at journal resolution.
#' The format comes from the file extension. Sizes are in millimetres; the
#' presets match common journal column widths: `"single"` (85 mm wide),
#' `"double"` (170 mm, the default) and `"slide"` (254 x 143 mm, 16:9).
#'
#' PNG and TIFF use the 'ragg' devices when 'ragg' is installed (better text
#' rendering, Japanese fonts work), otherwise `grDevices::png()` /
#' `grDevices::tiff()`. SVG uses 'svglite' when installed, otherwise
#' `grDevices::svg()`.
#'
#' @param plot A `ggplot` (or anything `print()` draws, e.g. a 'patchwork').
#' @param file Output path ending in `.png`, `.tif`, `.tiff` or `.svg`.
#' @param size Size preset: `"double"`, `"single"` or `"slide"`. Ignored for
#'   a dimension given in `width` / `height`.
#' @param width,height Size in `units`. Default: from `size` (the height is
#'   0.75 of the width for the column presets).
#' @param units `"mm"`, `"cm"` or `"in"`.
#' @param dpi Resolution of PNG and TIFF (300 by default; 600 for line art).
#' @param bg Background colour.
#' @return `file`, invisibly.
#' @examples
#' p <- sp_trend(sp_synth_claims(n = 200))
#' f <- tempfile(fileext = ".png")
#' sp_save(p, f, size = "single")
#' file.exists(f)
#' @export
sp_save <- function(plot, file, size = c("double", "single", "slide"), width = NULL,
                    height = NULL, units = c("mm", "cm", "in"), dpi = 300, bg = "white") {
  size <- match.arg(size)
  units <- match.arg(units)
  if (!is.character(file) || length(file) != 1) stop("`file` must be a single path.", call. = FALSE)
  sp_check_number(dpi, "dpi", min = 72)
  ext <- tolower(tools::file_ext(file))
  if (!ext %in% c("png", "tif", "tiff", "svg")) {
    stop("`file` must end in .png, .tif, .tiff or .svg, not \"", ext, "\".", call. = FALSE)
  }
  preset <- switch(size, single = c(85, 85 * 0.75), double = c(170, 170 * 0.75),
                   slide = c(254, 142.875))
  to_in <- c(mm = 1 / 25.4, cm = 1 / 2.54, `in` = 1)
  w <- if (is.null(width)) preset[1] / 25.4 else width * to_in[[units]]
  h <- if (is.null(height)) preset[2] / 25.4 else height * to_in[[units]]
  has_ragg <- requireNamespace("ragg", quietly = TRUE)

  if (ext == "png") {
    if (has_ragg) {
      ragg::agg_png(file, width = w, height = h, units = "in", res = dpi, background = bg)
    } else {
      grDevices::png(file, width = w, height = h, units = "in", res = dpi, bg = bg)
    }
  } else if (ext %in% c("tif", "tiff")) {
    if (has_ragg) {
      ragg::agg_tiff(file, width = w, height = h, units = "in", res = dpi, background = bg,
                     compression = "lzw")
    } else {
      grDevices::tiff(file, width = w, height = h, units = "in", res = dpi, bg = bg,
                      compression = "lzw")
    }
  } else if (requireNamespace("svglite", quietly = TRUE)) {
    svglite::svglite(file, width = w, height = h, bg = bg)
  } else {
    grDevices::svg(file, width = w, height = h, bg = bg)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
  invisible(file)
}
