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
                               expand = TRUE,
                               aspect = c("free", "fixed"),
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
                               scale.inset = 1,
                               na.rm = FALSE,
                               inherit.aes = FALSE) {

  l <- layer(
         geom = ggproto(NULL, GeomMagnify), # we clone because self$plot holds state
         mapping = mapping, data = data, stat = stat,
         position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
         params = list(na.rm = na.rm, expand = expand, aspect = aspect,
                       axes = axes,
                       proj = proj, shadow = shadow,
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
  optional_aes = c("x", "y", "width", "height", "to_x", "to_y", "to_width",
                  "to_height"),

  # note: these parameters do magic by computing Stat$parameters()
  compute_panel = function (self, data, scales, x = NULL, y = NULL,
                            width = NULL, height = NULL,
                            to_x = NULL, to_y = NULL, to_width = NULL,
                            to_height = NULL, aspect = NULL) {
    # missing/get/assign are weird, maybe due to ggproto? Hence this
    if (is.null(x)) x <- data$x
    if (is.null(y)) y <- data$y
    if (is.null(width)) width <- data$width
    if (is.null(height)) height <- data$height
    if (is.null(to_x)) to_x <- data$to_x
    if (is.null(to_y)) to_y <- data$to_y
    if (is.null(to_width)) to_width <- data$to_width
    if (is.null(to_height)) to_height <- data$to_height

    data$xmin = x - width/2
    data$xmax = x + width/2
    data$ymin = y - height/2
    data$ymax = y + height/2
    data$to_xmin = to_x - to_width/2
    data$to_xmax = to_x + to_width/2
    data$to_ymin = to_y - to_height/2
    data$to_ymax = to_y + to_height/2

    ggproto_parent(StatMagnify, self)$compute_panel(data = data, scales = scales)
  }
)

