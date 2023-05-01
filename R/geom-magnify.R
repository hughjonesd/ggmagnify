
#' @import ggplot2
#' @import grid
NULL


# TODO:
# - stop it going outside plot area (or not?)
# - add many doctests
# - bugs when you save a geom_magnify() plot to a variable, then add stuff
# - magnify should be an aes() if anything should
# - if you have aes() at all, it makes sense to allow multiple on one plot
# - some automatic way to find an empty area, cf. ggrepel?
# - shape = "circle":
#   - circular zoom lines
#   - mask the inset
# - deal with coord_fixed, or find some way to make "real circles":
#   ggforce::geom_ellipse?

#' Magnified inset of a plot
#'
#' `geom_magnify()` creates a magnified inset of part of a plot. The target
#' area is centred at `(x,y)` with `width` and `height`. The inset is centred
#' at `(to_x,to_y)`, with `width` and `height` multiplied by `magnify`. Optional
#' borders are drawn around the target and inset, along with projection lines
#' from one to the other.
#'
#' @inherit ggplot2::layer params
#' @inherit ggplot2::geom_point params
#' @param magnify Numeric. How much to magnify the target area? Give a length 2
#'   vector for separate x- and y-magnification.
#' @param expand Logical. Expand the limits of the target area by a small factor
#'   in the inset plot. See [coord_cartesian()][ggplot2::coord_cartesian()].
#' @param proj String. What style of projection lines to draw? `"facing"` (the
#'   default), `"corresponding"` or `"single"`. Can be abbreviated. See below.
#' @param linetype,alpha,linewidth Linetype, alpha and linewidth for borders
#'   and projection lines. Set `linetype = 0` for no lines.
#' @param target.linetype,inset.linetype,proj.linetype Linetypes
#'   for specific components.
#' @param shape Not yet implemented.
#' @param plot Ggplot object to plot in the inset. If `NULL`, defaults to the
#'   ggplot object to which `geom_magnify()` is added.
#' @param recalculate Logical. If `TRUE`, use [lims()][ggplot2::lims()] to
#'   replot the inset. Statistics, e.g. smoothing lines, will be
#'   recalculated using only the data in the target area. If `FALSE`, use
#'   [coord_cartesian()][ggplot2::coord_cartesian()] to replot the inset,
#'   keeping all the data.
#'
#' @section Aesthetics:
#' geom_magnify understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x**
#' - **y**
#' - **width**
#' - **height**
#' - **to_x**
#' - **to_y**
#' - colour
#'
#' @export
#'
#' @doctest
#' library(ggplot2)
#' ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
#'          geom_point() + xlim(c(2, 6))
#' ggp
#'
#' @expect silent()
#' ggp + geom_magnify(x = 3, width = 1, y = 6.5, height = 1,
#'                    to_x = 5, to_y = 5, magnify = 1.5)
#' @expect silent()
#' ggp +
#'       facet_wrap(vars(Species)) +
#'       geom_magnify(x = 3, width = 1, y = 6.5, height = 1,
#'                    to_x = 5, to_y = 5, magnify = 1.5)
geom_magnify <- function (mapping = NULL, data = NULL,  stat = "identity",
                          position = "identity", ...,
                          magnify,
                          expand = TRUE,
                          proj = c("facing", "corresponding", "single"),
                          linetype = 1,
                          target.linetype = linetype,
                          inset.linetype = linetype,
                          proj.linetype = 2,
                          alpha = 1,
                          linewidth = 0.5,
                          shape = "rect",
                          plot = NULL,
                          recalculate = FALSE,
                          na.rm = FALSE,
                          inherit.aes = FALSE) {
  if (! missing(data)) {
    cli::cli_abort("{.fn geom_magnify} cannot use its own data")
  }

  if (missing(magnify)) {
    cli::cli_abort("{.f geom_magnify} requires {.code magnify} to be set")
  }
  if (length(magnify) == 1L) {
    magnify <- c(magnify, magnify)
  }
  proj <- match.arg(proj)

  l <- layer(
         geom = ggproto(NULL, GeomMagnify), # we clone because self$plot holds state
         mapping = mapping, data = data, stat = stat,
         position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
         params = list(na.rm = na.rm, magnify = magnify, expand = expand,
                       recalculate = recalculate, proj = proj,
                       linewidth = linewidth, linetype = linetype,
                       target.linetype = target.linetype,
                       proj.linetype = proj.linetype,
                       inset.linetype = inset.linetype,
                       shape = shape, alpha = alpha, plot = plot, ...)
       )
  class(l) <- c("GeomMagnifyS3", class(l))

  l
}


#' @export
GeomMagnify <- ggproto("GeomMagnify", Geom,
  required_aes = c("x", "y", "height", "width", "to_x", "to_y"),
  default_aes = aes(colour = "black"),
  draw_key = draw_key_blank,
  plot = NULL,
  rename_size = FALSE,

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
  },

  draw_panel = function (self, data, panel_params, coord,
                         magnify, recalculate, proj,
                         linetype, target.linetype, proj.linetype, inset.linetype,
                         linewidth, alpha, shape, expand, plot
                         ) {
    d1 <- data[1, , drop = FALSE]   # untransformed, for other geoms

    # == draw borders around target and inset ==
    target_df <- data.frame(xmin = d1$xmin, xmax = d1$xmax, ymin = d1$ymin,
                            ymax = d1$ymax,
                            colour = d1$colour, fill = NA,
                            linewidth = linewidth,
                            linetype = target.linetype, alpha = alpha)
    target_grob <- if (shape == "rect") {
                     GeomRect$draw_panel(target_df, panel_params, coord)
                   } else {
                     make_geom_circle_grob(target_df, panel_params, coord)
                   }

    border_df <- data.frame(xmin = d1$to_xmin, xmax = d1$to_xmax, ymin = d1$to_ymin,
                            ymax = d1$to_ymax,
                            colour = d1$colour, fill = NA,
                            linewidth = linewidth,
                            linetype = inset.linetype, alpha = alpha)
    border_grob <- if (shape == "rect") {
                     GeomRect$draw_panel(border_df, panel_params, coord)
                   } else {
                     make_geom_circle_grob(border_df, panel_params, coord)
                   }


    # == draw projection lines ==========================================

    proj_df <- calculate_proj_segments(
                 proj = proj, shape = shape,
                 xmin = d1$xmin, xmax = d1$xmax,
                 ymin = d1$ymin, ymax = d1$ymax,
                 to_xmin = d1$to_xmin, to_xmax = d1$to_xmax,
                 to_ymin = d1$to_ymin, to_ymax = d1$to_ymax
               )
    proj_df$colour <- d1$colour
    proj_df$alpha <- alpha
    proj_df$linewidth <- linewidth
    proj_df$linetype <- proj.linetype
    proj_grob <- GeomSegment$draw_panel(proj_df, panel_params, coord)


    # == draw the magnified plot ========================================
    plot <- plot %||% self$plot

    new_lims <- if (recalculate) {
      lims(x = c(d1$xmin, d1$xmax), y = c(d1$ymin, d1$ymax))
    } else {
      coord_cartesian(xlim = c(d1$xmin, d1$xmax), ylim = c(d1$ymin, d1$ymax),
                        expand = expand)
    }

    suppressWarnings({
      plot <- plot + new_lims + inset_theme(axes = FALSE)
    })

    panel_id <- as.numeric(data$PANEL[1])
    plot_built <- ggplot_build(plot)
    layout_df <- plot_built$layout$layout
    layout_df <- layout_df[as.numeric(layout_df$PANEL) == panel_id,]
    if (nrow(layout_df) > 0) {
      layout_df$PANEL <- factor(1)
      layout_df$ROW <- 1
      layout_df$COL <- 1
    } else {
      layout_df$PANEL <- factor(levels = 1)
    }

    plot_built$layout$layout <- layout_df

    for (i in seq_along(plot_built$data)) {
      pbd <- plot_built$data[[i]]
      pbd <- pbd[as.numeric(pbd$PANEL) == panel_id,]
      pbd$PANEL <- if (nrow(pbd) > 0) factor(1) else factor(levels = 1)
      plot_built$data[[i]] <- pbd
    }

    plot_gtable <- ggplot_gtable(plot_built)

    if (shape == "circle") {
      corners <- data.frame(
        x = c(d1$to_xmin, d1$to_xmax),
        y = c(d1$to_ymin, d1$to_ymax)
      )
      corners <- coord$transform(corners, panel_params)

      x_rng <- range(corners$x, na.rm = TRUE)
      y_rng <- range(corners$y, na.rm = TRUE)

      vp <- viewport(x = mean(x_rng), y = mean(y_rng), width = diff(x_rng),
                     height = diff(y_rng), default.units = "native",
                     # here "native" means native to *this* viewport!
                     mask = circleGrob(x = 0.5, y = 0.5, r = 0.5,
                                       default.units = "native",
                                       gp = gpar(fill=rgb(0, 0, 0, 1))))

      plot_grob <- grid::editGrob(plot_gtable, vp = vp)
    } else {
      plot_grob <- GeomCustomAnn$draw_panel(data, panel_params, coord,
                                            grob = plot_gtable,
                                            xmin = d1$to_xmin, xmax = d1$to_xmax,
                                            ymin = d1$to_ymin, ymax = d1$to_ymax)
    }

    grid::gTree(name = paste("ggmagnify", annotation_id()),
          children = gList(target_grob, proj_grob, plot_grob, border_grob))
  },
)


make_geom_circle_grob <- function (df, panel_params, coord) {
      df <- transform(df,
                             x0 = (xmin + xmax)/2,
                             y0 = (ymin + ymax)/2,
                             r = min(xmax - xmin, ymax - ymin),
                             group = 1)
      df <- ggforce::StatCircle$compute_panel(df, NULL)
      ggforce::GeomCircle$draw_panel(df, panel_params,coord)
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


plot_clone <- function (plot) {
    p <- plot
    p$scales <- plot$scales$clone()
    p
}
