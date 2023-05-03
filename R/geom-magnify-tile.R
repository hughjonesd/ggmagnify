#' @include geom-magnify.R

#' @rdname geom_magnify
#' @param magnify Numeric. How much to magnify the target area? Give a length 2
#'   vector for separate x- and y-magnification.
#' @details
#' `geom_magnify_tile()` is a version of `geom_magnify()` which uses different
#' aesthetics. Set `x`, `width`, `y`, `height` and `magnify`
#' to specify the target and inset location.
#'
#' @export
geom_magnify_tile <- function (mapping = NULL, data = NULL,  stat = "identity",
                              position = "identity", ...,
                              magnify,
                              expand = TRUE,
                              axes = "",
                              proj = "facing",
                              shadow = FALSE,
                              linetype = 1,
                              target.linetype = linetype,
                              inset.linetype = linetype,
                              proj.linetype = 2,
                              alpha = 1,
                              linewidth = 0.4,
                              shape = "rect",
                              plot = NULL,
                              shadow.args = list(sigma = 5, colour = "grey40",
                                                 x_offset = 5, y_offset = 5),
                              recompute = FALSE,
                              na.rm = FALSE,
                              inherit.aes = FALSE) {
  if (missing(magnify)) {
    cli::cli_abort("{.f geom_magnify} requires {.code magnify} to be set")
  }
  if (length(magnify) == 1L) {
    magnify <- c(magnify, magnify)
  }

  l <- layer(
         geom = ggproto(NULL, GeomMagnify2), # we clone because self$plot holds state
         mapping = mapping, data = data, stat = stat,
         position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
         params = list(na.rm = na.rm, magnify = magnify, expand = expand,
                       axes = axes,
                       proj = proj, shadow = shadow,
                       linewidth = linewidth, linetype = linetype,
                       target.linetype = target.linetype,
                       proj.linetype = proj.linetype,
                       inset.linetype = inset.linetype,
                       shape = shape, alpha = alpha, plot = plot,
                       shadow.args = shadow.args, recompute = recompute, ...)
       )
  class(l) <- c("GeomMagnifyLayer", class(l))

  l
}


#' @rdname GeomMagnify
#' @format NULL
#' @usage NULL
#' @export
GeomMagnify2 <- ggproto("GeomMagnify2", GeomMagnify,
  required_aes = c("x", "y", "height", "width", "to_x", "to_y"),

  setup_data = function (self, data, params) {
    for (v in c("x", "y", "to_x", "to_y", "width", "height")) {
      data[[v]] <- data[[v]] %||% params[[v]]
    }

    magnify <- params$magnify
    magnify_x <- magnify[1]
    magnify_y <- magnify[2]
    transform(data,
              xmin = x - width/2,
              xmax = x + width/2,
              ymin = y - height/2,
              ymax = y + height/2,
              to_xmin = to_x - width * magnify_x/2,
              to_xmax = to_x + width * magnify_x/2,
              to_ymin = to_y - height * magnify_y/2,
              to_ymax = to_y + height * magnify_y/2
              )
  }
)
