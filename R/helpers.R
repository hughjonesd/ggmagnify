
expand_by <- function (x, expand) {
  m <- mean(range(x))
  (x - m) * (1 + expand) + m
}


#' Helper functions to find rectangles or convex hulls of data
#'
#' @param x,y Unquoted names of expressions
#' @param data A data frame
#' @param expand Amount to expand the data around its midpoint. Default is 10
#'   per cenet.
#'
#' @return `rect_around()` returns a list with names `xmin`, `xmax`, `ymin`,
#'   and `ymax`. `hull_around()` returns a data frame with columns `x` and `y`.
#'
#' @export
#'
#' @doctest
#'
#' library(ggplot2)
#' to <- c(2, 4.5, 6, 8)
#' setosas <- iris[iris$Species == "setosa", ]
#'
#' @expect silent()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point() +
#'   geom_magnify(to = to,
#'                from = rect_around(Sepal.Width, Sepal.Length,
#'                                   data = setosas))
#' @expect silent()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point() +
#'   geom_magnify(to = to,
#'                from = hull_around(Sepal.Width, Sepal.Length,
#'                                   data = setosas))
rect_around <- function (x, y, data = NULL, expand = 0.1) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  x <- rlang::eval_tidy(x, data = data)
  y <- rlang::eval_tidy(y, data = data)
  xr <- expand_by(range(x), expand)
  yr <- expand_by(range(y), expand)

  list(xmin = xr[1], xmax = xr[2], ymin = yr[1], ymax = yr[2])
}


#' @rdname rect_around
#' @export
hull_around <- function (x, y, data = NULL, expand = 0.1) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  x <- rlang::eval_tidy(x, data = data)
  y <- rlang::eval_tidy(y, data = data)

  ch <- grDevices::chull(x, y)
  x <- expand_by(x[ch], expand)
  y <- expand_by(y[ch], expand)

  data.frame(x = x, y = y)
}
