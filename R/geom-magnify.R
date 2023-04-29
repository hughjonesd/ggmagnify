
#' @import ggplot2
NULL

# TODO:
# - implement an "expand" option
# - allow magnify_x and y to be separate
# - alternative parametrization where you specify xlim/ylim/to_xlim/to_ylim
#   separately? (Inheritance could help here...)


#' @export
GeomMagnify <- ggproto("GeomMagnify", Geom,
  required_aes = c("x", "y", "height", "width", "to_x", "to_y", "magnify"),
  default_aes = aes(colour = "black", linetype = 1, linewidth = 0.5, shape = "rect"),
  draw_key = draw_key_blank,

  setup_params = function(self, data, params) {
    first_row <- data[1, ]
    for (v in c("width", "height", "x", "y", "to_x", "to_y", "magnify")) {
      params[[v]] <- params[[v]] %||% first_row[[v]]
    }

    new_params <- with(params, list(
              xmin = x - width/2,
              xmax = x + width/2,
              ymin = y - height/2,
              ymax = y + height/2,
              to_xmin = to_x - width * magnify/2,
              to_xmax = to_x + width * magnify/2,
              to_ymin = to_y - height * magnify/2,
              to_ymax = to_y + height * magnify/2
              ))
    c(params, new_params)
  },

  draw_layer = function(self, data, params, layout, coord) {
    data$GgMagnifyRow <- seq_len(nrow(data))
    ggproto_parent(Geom, self)$draw_layer(data, params, layout, coord)
  },

  draw_panel = function (self, data, panel_params, coord, xmin, xmax,
                         ymin, ymax, to_xmin, to_xmax, to_ymin, to_ymax) {
    plot <- self$plot %+% self$plot$data[data$GgMagnifyRow, ]
    suppressWarnings({
      plot <- plot +
        coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
        facet_null() +
        inset_theme(axes = FALSE) +
        theme(plot.background = element_rect(colour = "black", linewidth = 4))
    })

    inset_grob <- ggplotGrob(plot)

    corners <- data.frame(
      x = c(to_xmin, to_xmax),
      y = c(to_ymin, to_ymax)
    )

    data <- coord$transform(corners, panel_params)

    x_rng <- range(data$x, na.rm = TRUE)
    y_rng <- range(data$y, na.rm = TRUE)

    vp <- grid::viewport(x = mean(x_rng), y = mean(y_rng),
                   width = diff(x_rng), height = diff(y_rng),
                   just = c("center","center"))

    grid::editGrob(inset_grob, vp = vp, name = paste(inset_grob$name, annotation_id()))
  },
)

#' #' @export
#' StatMagnify <- ggproto("StatMagnify", Stat,
#'   compute_layer = function (self, data, scales, ...) {
#'     browser()
#'     data
#'   }
#' )

# only reason to have multiple data-dependent values would be in a facet
# otherwise it's more like abline

#' @export
geom_magnify <- function (mapping = NULL, data = NULL,  stat = "identity",
                          position = "identity", ..., na.rm = FALSE) {
  l <- layer(
         geom = GeomMagnify, mapping = mapping,  data = data, stat = stat,
         position = "identity", show.legend = FALSE, inherit.aes = TRUE,
         params = list(na.rm = na.rm, ...)
       )
  class(l) <- c("GeomMagnifyS3", class(l))
  l
}

#' @export
ggplot_add.GeomMagnifyS3 <- function(object, plot, object_name) {
  object$geom$plot <- plot
  NextMethod()
}

annotation_id <- local({
  i <- 1
  function() {
    i <<- i + 1
    i
  }
})
