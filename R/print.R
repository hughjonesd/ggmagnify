
#' Print a GgMagnify object
#'
#' @param x A GgMagnify object.
#' @param ... Passed on to [ggplot2::print.ggplot()].
#'
#' @return The GgMagnify object, invisibly.
#' @export
print.GgMagnify <- function (x, ...) {
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

  oldx <- x
  x <- x$plot + x$target + shadow + x$proj + inset + x$border
  print(x) # for some reason NextMethod doesn't work here

  return(invisible(oldx))
}
