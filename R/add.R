
#' Deprecated
#'
#' @param e1 A GgMagnify object
#' @param e2 A layer, theme, etc.
#'
#' @return The modified object
#' @export
`+.GgMagnify` <- function (e1, e2) {
  e1$plot <- e1$plot + e2
  e1$inset <- e1$inset + e2

  e1
}

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(chooseOpsMethod, GgMagnify)
chooseOpsMethod.GgMagnify <- function(x, y, mx, my, cl, reverse) {
  TRUE
}
