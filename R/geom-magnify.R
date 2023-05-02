
#' @import ggplot2
#' @import grid
NULL


# TODO:
# - make binned & discrete scales work
# - bugs when you save a geom_magnify() plot to a variable, then add stuff?
# - magnify should be an aes() if anything should
#   - but more generally, are there simpler ways to specify stuff?
#   - to_height, to_width? These actually make more sense, because if you
#     add axes, then magnify quickly becomes misleading
#   - and also then they're easy to make aesthetics; magnify could stay
#     as a param.
#   - a GeomMagnifyRect from which the main one inherits? Cheap way to get
#     an alternative interface.
# - if you have aes() at all, it makes sense to allow multiple on one plot
# - some automatic way to find an empty area, cf. ggrepel?
# - coord_sf, coord_fixed and others ***
# - allow axes (not for "ellipse")
# - be more independent. You can just draw your own border round
#   the viewport
# - new readme
# - maybe complex example and tweak doctest to allow doctestFile
# -make it work with log scales... if you can, ha ha

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
#' @param axes Logical. Draw axes in the inset?
#' @param proj String. What style of projection lines to draw? `"facing"` (the
#'   default), `"corresponding"` or `"single"`. Can be abbreviated. See below.
#' @param shadow Logical. Draw a shadow behind the inset plot? Requires the
#'   "ggfx" package.
#' @param linetype,alpha,linewidth Linetype, alpha and linewidth for borders
#'   and projection lines. Set `linetype = 0` for no lines.
#' @param target.linetype,inset.linetype,proj.linetype Linetypes
#'   for specific components.
#' @param shape `"rect"` to magnify a rectangle. `"ellipse"` to magnify an ellipse.
#'   `"ellipse` requires the "ggforce" package.
#' @param plot Ggplot object to plot in the inset. If `NULL`, defaults to the
#'   ggplot object to which `geom_magnify()` is added.
#' @param shadow.args List. Arguments to [ggfx::with_shadow()].
#' @param recompute Logical. If `TRUE`, use [lims()][ggplot2::lims()] to
#'   replot the inset. Statistics, e.g. smoothing lines, will be
#'   recomputed using only the data in the target area. If `FALSE`, use
#'   [coord_cartesian()][ggplot2::coord_cartesian()] to replot the inset,
#'   keeping all the data.
#'
#' ## Aesthetics
#'
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
#' Normally you'll set these directly in the call to `geom_magnify()`, but
#' you can use them to change the magnification area, e.g. by facet. *Note:*
#' as of today, the code only allows one magnification per panel.
#'
#' ## Projection lines
#'
#' `proj = "corresponding"` or `"facing"` draws projection lines from the
#' corners of the target to the corners of the inset. `"corresponding"` always
#' projects each corner of the target to the same corner of the inset.
#' `"facing"` sometimes draws lines between facing corners, when this looks
#' cleaner. `"single"` draws a single line from the midpoint of facing sides.
#' `"none"` draws no lines.
#'
#' @export
#'
#' @doctest
#' library(ggplot2)
#' ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
#'          geom_point() + xlim(c(2, 6))
#'
#' # Basic magnification
#' @expect silent()
#' ggp + geom_magnify(x = 3, width = 1, y = 6.5, height = 1,
#'                    to_x = 5, to_y = 5, magnify = 1.5)
#'
#' # Inset axes
#' ggp + geom_magnify(x = 3, width = 1, y = 6.5, height = 1,
#'                    to_x = 4, to_y = 5, magnify = c(3, 1.5), axes = TRUE)
#'
#' # Ellipse magnification
#' @expect silent()
#' if (requireNamespace("ggforce", quietly = TRUE) && getRversion() >= 4.2) {
#'   ggp + geom_magnify(x = 3, width = 1, y = 6.5, height = 1,
#'                      to_x = 5, to_y = 5, magnify = 1.5, shape = "ellipse")
#' }
#'
#' # Shadow
#' @expect silent()
#' if (requireNamespace("ggfx", quietly = TRUE)) {
#'   ggp + geom_magnify(x = 3, width = 1, y = 6.5, height = 1,
#'                      to_x = 5, to_y = 5, magnify = 1.5, shadow = TRUE)
#' }
#'
#' # Order matters
#'
#' # `geom_magnify()` stores the plot when it is added to it:
#' @expect no_error()
#' ggp + geom_smooth() + geom_magnify(x = 3, width = 1, y = 6.5, height = 1,
#'                    to_x = 5, to_y = 5, magnify = 1.5)
#'
#' # This will print the inset without the smooth:
#' @expect no_error()
#' ggp + geom_magnify(x = 3, width = 1, y = 6.5, height = 1,
#'                    to_x = 5, to_y = 5, magnify = 1.5) + geom_smooth()
#'
geom_magnify <- function (mapping = NULL, data = NULL,  stat = "identity",
                          position = "identity", ...,
                          magnify,
                          expand = TRUE,
                          axes = FALSE,
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
         geom = ggproto(NULL, GeomMagnify), # we clone because self$plot holds state
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
  class(l) <- c("GeomMagnifyS3", class(l))

  l
}

#' @rdname geom_magnify
#' @details
#' `geom_magnify2()` is a version of `geom_magnify()` which uses different
#' aesthetics. Set `xmin`, `xmax`, `ymin`, `ymax` and `to_xmin`, `to_xmax` etc.
#' to specify the target and inset location.
#'
#' @export
geom_magnify2 <- function (mapping = NULL, data = NULL,  stat = "identity",
                           position = "identity", ...,
                           expand = TRUE,
                           axes = FALSE,
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

  l <- layer(
         geom = ggproto(NULL, GeomMagnify2), # we clone because self$plot holds state
         mapping = mapping, data = data, stat = stat,
         position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
         params = list(na.rm = na.rm, expand = expand,
                       axes = axes,
                       proj = proj, shadow = shadow,
                       linewidth = linewidth, linetype = linetype,
                       target.linetype = target.linetype,
                       proj.linetype = proj.linetype,
                       inset.linetype = inset.linetype,
                       shape = shape, alpha = alpha, plot = plot,
                       shadow.args = shadow.args, recompute = recompute, ...)
       )
  class(l) <- c("GeomMagnifyS3", class(l))

  l
}



#' @rdname GeomMagnify
#' @format NULL
#' @usage NULL
#' @export
GeomMagnify2 <- ggproto("GeomMagnify2", Geom,
  required_aes = c("xmin", "xmax", "ymin", "ymax",
                   "to_xmin", "to_xmax", "to_ymin", "to_ymax"),
  default_aes = aes(colour = "black"),
  draw_key = draw_key_blank,
  plot = NULL,
  rename_size = FALSE,

  setup_params = function (data, params) {
    params$proj <- match.arg(params$proj, c("facing", "corresponding", "single"))
    params$shape <- match.arg(params$shape, c("rect", "ellipse"))
    if (params$shape == "ellipse") {
      rlang::check_installed("ggforce")
    }
    if (params$axes && params$shape == "ellipse") {
      cli::cli_warn(paste("Setting {.code axes} to {.code FALSE} with",
                          "{.code shape = \"ellipse\"}"))
      params$axes <- FALSE
    }

    params
  },

  draw_panel = function (self, data, panel_params, coord,
                         magnify, axes, proj, shadow,
                         linetype, target.linetype, proj.linetype, inset.linetype,
                         linewidth, alpha, shape, expand, plot, shadow.args, recompute
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
                     make_geom_ellipse_grob(target_df, panel_params, coord)
                   }

    border_df <- data.frame(xmin = d1$to_xmin, xmax = d1$to_xmax, ymin = d1$to_ymin,
                            ymax = d1$to_ymax,
                            colour = d1$colour, fill = NA,
                            linewidth = linewidth,
                            linetype = inset.linetype, alpha = alpha)
    border_grob <- if (shape == "rect") {
                     GeomRect$draw_panel(border_df, panel_params, coord)
                   } else {
                     make_geom_ellipse_grob(border_df, panel_params, coord)
                   }


    # == draw projection lines ==========================================

    proj_df <- calculate_proj_segments(
                 proj = proj, shape = shape,
                 xmin = d1$xmin, xmax = d1$xmax,
                 ymin = d1$ymin, ymax = d1$ymax,
                 to_xmin = d1$to_xmin, to_xmax = d1$to_xmax,
                 to_ymin = d1$to_ymin, to_ymax = d1$to_ymax
               )
    proj_grob <- if (is.null(proj_df)) NULL else {
      proj_df$colour <- d1$colour
      proj_df$alpha <- alpha
      proj_df$linewidth <- linewidth
      proj_df$linetype <- proj.linetype
      GeomSegment$draw_panel(proj_df, panel_params, coord)
    }

    # == draw the magnified plot ========================================
    plot <- plot %||% self$plot

    plot_limits <- ggplot_build(plot)$layout$coord$limits
    rev_x <- ! is.null(plot_limits$x) && diff(plot_limits$x) < 0
    rev_y <- ! is.null(plot_limits$y) && diff(plot_limits$y) < 0
    xlim_vals <- c(d1$xmin, d1$xmax)
    ylim_vals <- c(d1$ymin, d1$ymax)
    if (rev_x) xlim_vals <- xlim_vals[2:1]
    if (rev_y) ylim_vals <- ylim_vals[2:1]

    new_lims <- if (recompute) {
      lims(x = xlim_vals, y = ylim_vals)
    } else {
      coord_cartesian(xlim = xlim_vals, ylim = ylim_vals,
                        expand = expand)
    }

    suppressMessages({
      plot <- plot + new_lims + inset_theme(axes = axes)
    })

    plot_built <- ggplot_build(plot)

    # DARK MAGIC HERE ------------------------------------------------
    panel_id <- as.numeric(data$PANEL[1])

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
    # END MAGIC -------------------------------------------------------

    plot_gtable <- ggplot_gtable(plot_built)

    corners <- data.frame(
      x = c(d1$to_xmin, d1$to_xmax),
      y = c(d1$to_ymin, d1$to_ymax)
    )
    corners_t <- coord$transform(corners, panel_params)

    x_rng <- range(corners_t$x, na.rm = TRUE)
    y_rng <- range(corners_t$y, na.rm = TRUE)
    mask <- if (shape == "ellipse") {
      ellipse_df <- data.frame(xmin = 0, xmax = 1,
                               ymin = 0, ymax = 1)
      el_pts <- ellipse_points(ellipse_df)
      grid::polygonGrob(x = el_pts$x, y = el_pts$y,
                        default.units = "native",
                        gp = gpar(fill = rgb(0,0,0,1)))
    } else {
      grid::rectGrob(default.units = "native",
                     gp = gpar(fill = rgb(0,0,0,1)))
    }

    vp <- viewport(x = mean(x_rng), y = mean(y_rng), width = diff(x_rng),
                     height = diff(y_rng), default.units = "native",
                     mask = mask)
    plot_gtable <- grid::editGrob(plot_gtable, vp = vp)

    if (shadow) {
      rlang::check_installed("ggfx")
      plot_gtable <- do.call(ggfx::with_shadow, c(list(x = plot_gtable), shadow.args))
    }

    grid::gTree(name = paste("ggmagnify", annotation_id()),
          children = gList(target_grob, proj_grob, plot_gtable, border_grob))
  },
)


#' Internals
#'
#' @format NULL
#' @usage NULL
#' @export
GeomMagnify <- ggproto("GeomMagnify", GeomMagnify2,
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


make_geom_ellipse_grob <- function (df, panel_params, coord) {
  el_pts <- ellipse_points(df)
  ggforce::GeomCircle$draw_panel(el_pts, panel_params, coord)
}


ellipse_points <- function(df) {
  ellipse_df <- transform(df,
                x0 = (xmin + xmax)/2,
                y0 = (ymin + ymax)/2,
                a =  (xmax - xmin)/2,
                b =  (ymax - ymin)/2,
                angle = 0,
                group = 1)
  ellipse_df <- ggforce::StatEllip$setup_data(ellipse_df)
  el_pts <- ggforce::StatEllip$compute_panel(data = ellipse_df, scales = NULL)

  el_pts
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
