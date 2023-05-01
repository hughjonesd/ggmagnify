
#' Default elements to blank in the ggmagnify inset
#'
#' @param ... Character vector of extra elements to blank.
#' @param axes Logical. `TRUE` if the inset will include axes.
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
  blank_axes <- ! axes
  if (blank_axes) {
    axis_bits <- c(outer(c("axis.text", "axis.ticks", "axis.line"),
                       c("", ".x", ".y"), paste0))
    res <- c(res, axis_bits)
  }

  res
}


#' Create a theme suitable for an inset ggplot
#'
#' @param blank Character vector of extra elements to blank.
#'   See [ggplot2::theme()].
#' @param axes Logical: will the inset have axes?
#' @inherit ggmagnify params
#'
#' @return A ggplot theme object
#' @export
inset_theme <- function (
    blank = inset_blanks(axes = axes),
    axes,
    margin = if (axes) 10 else 0
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
  thm <- thm + ggplot2::theme(plot.margin = margin)

  if (! axes) {
    thm <- thm + ggplot2::theme(
      axis.ticks.length   = grid::unit(0, "pt"),
      axis.ticks.length.x = grid::unit(0, "pt"),
      axis.ticks.length.y = grid::unit(0, "pt")
    )
  }

  thm
}
