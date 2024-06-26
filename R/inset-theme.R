

#' Create a theme suitable for an inset ggplot
#'
#' Use `inset_theme()` to add a suitable theme to a manually-created inset
#' plot. Use `inset_blanks()` to customize the default list of elements to
#' blank in the inset.
#'
#' @param blank Character vector of theme elements to blank. See [ggplot2::theme()].
#' @param margin Margin around the plot. See `plot.margin` in [ggplot2::theme()].
#' @inherit geom_magnify params
#'
#' @return `inset_theme()` returns a ggplot theme object. `inset_blanks()`
#'   returns a character vector of theme element names.
#' @export
#' @doctest
#' library(ggplot2)
#' ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
#'          geom_point() + xlim(c(2, 6))
#' from <- list(2.5, 3.5, 6, 7)
#' to <- list(4, 6, 5, 7)
#'
#' blanks <- inset_blanks("panel.grid")
#' inset <- ggp + geom_density2d() + inset_theme(axes = "", blank = blanks)
#' @expect silent()
#' ggp + geom_magnify(from = from, to = to, plot = inset)
inset_theme <- function (
    blank = inset_blanks(axes = axes),
    axes = c("", "x", "y", "xy"),
    margin = if (axes == "") 0 else 8
  ) {
  # We put this before using `blank`, since it is part of the default argument.
  axes <- rlang::arg_match(axes)
  blank_elements <- lapply(blank, function (x) {
    ggplot2::element_blank()
  })
  names(blank_elements) <- blank
  blank_elements[["legend.position"]] <- "none"
  thm <- do.call(ggplot2::theme, blank_elements)

  if (! inherits(margin, "unit")) {
    if (length(margin) == 1) margin <- rep(margin, 4)
    margin <- grid::unit(margin, "pt")
  }
  thm <- thm + theme(plot.margin = margin)

  if (axes == "") thm <- thm + theme(axis.ticks.length = grid::unit(0, "pt"))
  if (axes %in% c("", "y")) thm <- thm + theme(axis.ticks.length.x = grid::unit(0, "pt"))
  if (axes %in% c("", "x")) thm <- thm + theme(axis.ticks.length.y = grid::unit(0, "pt"))

  thm
}


#' @param ... Character vector of extra elements to blank.
#' @inherit geom_magnify params
#'
#' @export
#' @rdname inset_theme
inset_blanks <- function (..., axes = c("", "x", "y", "xy")) {
  res <- c("plot.title", "plot.subtitle", "plot.caption", "plot.tag",
              "axis.title", "axis.title.x", "axis.title.y",
              "strip.background", "strip.background.x", "strip.background.y",
              "strip.text", "strip.text.x", "strip.text.y",
              "strip.text.x.bottom", "strip.text.x.top",
              "strip.text.y.left", "strip.text.y.right",
           ...)
  axes <- rlang::arg_match(axes)
  axis_suffixes_to_blank <- switch(paste0("+", axes), # avoid "zero-length name"
                                   "+" = c("", ".x", ".y"),
                                   "+x" = ".y",
                                   "+y" = ".x",
                                   "+xy" = character(0)
                                    )
  axis_bits <- c(outer(c("axis.text", "axis.ticks", "axis.line"),
                     axis_suffixes_to_blank, paste0))
  res <- c(res, axis_bits)

  res
}
