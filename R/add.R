
#' Add layer to a GgMagnify object
#'
#' This adds the layer to *both* the original plot, *and* the inset.
#'
#' @param e1 A GgMagnify object
#' @param e2 A layer, theme, etc.
#'
#' @return The modified object
#' @export
#' @example man/R/advanced-example.R
`+.GgMagnify` <- function (e1, e2) {
  e1$plot <- e1$plot + e2
  e1$inset <- e1$inset + e2

  e1
}

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(chooseOpsMethod, GgMagnify)
chooseOpsMethod.GgMagnify <- function(x, y, mx, my, cl, reverse) {
  TRUE
}
