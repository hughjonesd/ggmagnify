
#' Default elements to blank in the ggmagnify inset
#'
#' @param ... Character vector of extra elements to blank.
#' @param axes Logical. `TRUE` if the inset will include axes.
#'
#' @return A character vector.
#' @export
inset_blanks <- function (..., axes) {
  res <- c("plot.title", "plot.subtitle", "plot.caption", "plot.tag",
              "axis.title", ...)
  blank_axes <- ! axes
  if (blank_axes) res <- c(res, "axis.text", "axis.ticks", "axis.line")

  res
}


#' Create a theme suitable for an inset ggplot
#'
#' @param blank Character vector of extra elements to blank.
#'   See [ggplot2::theme()].
#' @param axes Logical: will the inset have axes? Ignored if `blank` is
#'   set.
#'
#' @return A ggplot theme object
#' @export
inset_theme <- function (blank = inset_blanks(axes = axes), axes) {
  blank_elements <- lapply(blank, function (x) {
    ggplot2::element_blank()
  })
  names(blank_elements) <- blank
  blank_elements[["legend.position"]] <- "none"
  thm <- do.call(ggplot2::theme, blank_elements)

  thm
}
