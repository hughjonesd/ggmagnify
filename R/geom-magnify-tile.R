#' @include geom-magnify.R

#' @rdname geom_magnify
#' @details
#' `geom_magnify_tile()` is a version of `geom_magnify()` which uses different
#' aesthetics. Set `x`, `width`, `y`, `height` and `to_x`, `to_width` `to_y`,
#' `to_height` to specify the target and inset location.
#'
#' @export
geom_magnify_tile <- function (mapping = NULL,
                               data = NULL,
                               stat = StatMagnifyTile,
                               position = "identity",
                               ...,
                               expand = 0.1,
                               aspect = c("free", "fixed"),
                               axes = "",
                               proj = "facing",
                               shadow = FALSE,
                               colour = "black",
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
                               scale.inset = 1,
                               na.rm = FALSE,
                               inherit.aes = FALSE) {

  l <- layer(
         geom = ggproto(NULL, GeomMagnify), # we clone because self$plot holds state
         mapping = mapping, data = data, stat = stat,
         position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
         params = list(na.rm = na.rm, expand = expand, aspect = aspect,
                       axes = axes,
                       proj = proj, shadow = shadow, colour = colour,
                       linewidth = linewidth, linetype = linetype,
                       target.linetype = target.linetype,
                       proj.linetype = proj.linetype,
                       inset.linetype = inset.linetype,
                       shape = shape, alpha = alpha, plot = plot,
                       shadow.args = shadow.args, recompute = recompute,
                       scale.inset = scale.inset, ...)
       )
  class(l) <- c("GeomMagnifyLayer", class(l))

  l
}


StatMagnifyTile <- ggproto("StatMagnifyTile", StatMagnify,
  required_aes = c("x", "y", "width", "height", "to_x", "to_y", "to_width",
                  "to_height"),

  # note: these parameters do magic by computing Stat$parameters()
  compute_panel = function (self, data, scales, x = NULL, y = NULL,
                            width = NULL, height = NULL,
                            to_x = NULL, to_y = NULL, to_width = NULL,
                            to_height = NULL, aspect = NULL, expand) {
    xmin <- x - width/2
    xmax <- x + width/2
    ymin <- y - height/2
    ymax <- y + height/2
    to_xmin <- to_x - to_width/2
    to_xmax <- to_x + to_width/2
    to_ymin <- to_y - to_height/2
    to_ymax <- to_y + to_height/2

    from <- c(xmin, ymin, xmax, ymax)
    to <- c(to_xmin, to_ymin, to_xmax, to_ymax)
    data$from <- list(from)
    ggproto_parent(StatMagnify, self)$compute_panel(data = data, scales = scales,
                                                    from = from, to = to,
                                                    expand = expand)
  }
)

