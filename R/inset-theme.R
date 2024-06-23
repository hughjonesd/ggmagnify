
#' Default elements to blank in the ggmagnify inset
#'
#' @param ... Character vector of extra elements to blank.
#' @inherit geom_magnify params
#'
#' @return A character vector.
#' @export
inset_blanks <- function (..., axes) {
  res <- c("plot.title", "plot.subtitle", "plot.caption", "plot.tag",
              "axis.title", "axis.title.x", "axis.title.y",
              "strip.background", "strip.background.x", "strip.background.y",
              "strip.text", "strip.text.x", "strip.text.y",
              "strip.text.x.bottom", "strip.text.x.top",
              "strip.text.y.left", "strip.text.y.right",
           ...)

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


#' Create a theme suitable for an inset ggplot
#'
#' Use `inset_theme()` to add a suitable theme to a manually-created inset
#' plot.
#'
#' @param blank Character vector of theme elements to blank. See [ggplot2::theme()].
#' @param margin Margin around the plot. See `plot.margin` in [ggplot2::theme()].
#' @inherit geom_magnify params
#'
#' @return A ggplot theme object
#' @export
#' @doctest
#' library(ggplot2)
#' ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
#'          geom_point() + xlim(c(2, 6))
#' from <- list(2.5, 3.5, 6, 7)
#' to <- list(4, 6, 5, 7)
#'
#' inset <- ggp + geom_density2d() + inset_theme(axes = "")
#' @expect silent()
#' ggp + geom_magnify(from = from, to = to, plot = inset)
inset_theme <- function (
    blank = inset_blanks(axes = axes),
    axes,
    margin = if (axes == "") 0 else 8
  ) {
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
