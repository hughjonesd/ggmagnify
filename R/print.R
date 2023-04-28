
#' Print a GgMagnify object
#'
#' @param x A GgMagnify object.
#' @param ... Passed on to [ggplot2::print.ggplot()].
#'
#' @return The GgMagnify object, invisibly.
#' @export
print.GgMagnify <- function (x, ...) {
  print(compose(x)) # for some reason NextMethod didn't work here (but may now?)

  invisible(x)
}


#' @exportS3Method grid::grid.draw GgMagnify
grid.draw.GgMagnify <- function (x, recording = TRUE) {
  print(x)
}


#' Compose a GgMagnify object into a single ggplot
#'
#' Use this when you want to save the result of [ggmagnify()] to a file.
#'
#' @param x A GgMagnify object.
#' @param plot A ggplot object, by default the original plot.
#'
#' @return A ggplot
#' @export
#'
#' @doctest
#' library(ggplot2)
#' ggp <- ggplot(ChickWeight, aes(Time, weight, group = Chick, color = Diet)) +
#'   geom_line()
#'
#' ggm <- ggmagnify(ggp,
#'   xlim = c(0, 5), ylim = c(30, 80),
#'   inset_xlim = c(0, 15), inset_ylim = c(200, 350))
#'
#' @expect silent()
#' compose(ggm)
#'
#' @omit
#' \dontrun{
#'   ggsave("filename.png", compose(ggm))
#' }
compose <- function (x, plot = x$plot) {
  inset <- x$inset
  inset <- ggplot2::ggplotGrob(inset)
  shadow <- if (x$shadow) {
              shadow <- do.call(ggfx::with_shadow,
                                c(list(x = inset, stack = FALSE), x$shadow_args))
              ggplot2::annotation_custom(shadow, xmin = x$inset_xmin,
                                         xmax = x$inset_xmax,
                                         ymin = x$inset_ymin,
                                         ymax = x$inset_ymax)
            } else {
              NULL
            }
  inset <- ggplot2::annotation_custom(inset,
                                      xmin = x$inset_xmin, xmax = x$inset_xmax,
                                      ymin = x$inset_ymin, ymax = x$inset_ymax)

  plot + x$target + shadow + x$proj + inset + x$border
}
