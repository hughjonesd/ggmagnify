
#' @import ggplot2
#' @import grid
NULL

#' @include stat-magnify.R
NULL


# TODO:
# - make binned & discrete scales work (as and when ggplot2 faces reality...)
# - why isn't geom_abline() working when recompute = FALSE?
#
#
# NOT TODO
# - if you have aes() at all, it makes sense to allow multiple on one plot
#   - but it's a very rare use case and overplotting will become a pain...
#   - and it's liable to blow up when you put your aesthetics in the data.
#     Not worth it


#' Create a magnified inset of a plot
#'
#' `geom_magnify()` creates a magnified inset of part of a ggplot. Optional
#' borders are drawn around the target and inset, along with projection lines
#' from one to the other. `from` gives the location of the target area,
#' and `to` gives the location of the inset. Usually, these are specified as
#' `c(xmin, xmax, ymin, ymax)`.
#'
#' @inherit ggplot2::layer params
#' @param mapping,data,stat,position,...,na.rm See e.g. [ggplot2::geom_point()].
#' @param shape Shape of the area to be magnified. `"rect"` for a rectangle.
#'   `"ellipse"` for an ellipse. `"outline"` for the convex hull of points in the
#'   target area, or for map polygons.
#' @param aspect String. `"fixed"` to fix the aspect ratio (overrides `ymax`).
#' @param expand Number. Expand the target area and inset proportionally
#'   by this amount.
#' @param axes String. Which axes to plot in the inset? `""`, `"x"`, `"y"` or
#'   `"xy"`.
#' @param proj String. What style of projection lines to draw? `"facing"` (the
#'   default), `"corresponding"` or `"single"`. Can be abbreviated. See below.
#' @param shadow Logical. Draw a shadow behind the inset plot? Requires the
#'   "ggfx" package.
#' @param corners Numeric between 0 and 1. Radius of rounded corners for the
#'   target area and inset. Only used if `shape` is `"rect"`. 0.1 is a good
#'   starting value.
#' @param linetype,colour,alpha,linewidth Linetype, colour, alpha and linewidth
#' for borders and projection lines.
#' @param target.linetype,inset.linetype,proj.linetype Linetypes
#'   for specific components. Set to `0` for no lines.
#' @param plot Ggplot object to plot in the inset. If `NULL`, defaults to the
#'   ggplot object to which `geom_magnify()` is added.
#' @param shadow.args List. Arguments to [ggfx::with_shadow()].
#' @param recompute Logical. If `TRUE`, use [lims()][ggplot2::lims()] to
#'   replot the inset. Statistics, e.g. smoothing lines, will be
#'   recomputed using only the data in the target area. If `FALSE`, use
#'   [coord_cartesian()][ggplot2::coord_cartesian()] to replot the inset,
#'   keeping all the data.
#' @param scale.inset Length 1 or 2 numeric. Normally, exactly the target area
#'   is shown on the inset. Sometimes you may wish to rescale the plot in the
#'   inset. Use 2 numbers to scale width and height separately.
#' @param proj.combine Logical. How to draw projection lines when more than
#'   one polygon/map area is magnified? `FALSE` draws one set of projection
#'   lines for each area. `TRUE` draws a single set of lines for all the areas.
#'
#' @details
#' ## Aesthetics
#'
#' geom_magnify understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **from**
#' - **to**
#'
#' `from` and `to` can be vectors of length 4, like `list(xmin, xmax, ymin, ymax)`.
#' These specify the bottom left and top right corners of the target area to
#' magnify, and the area for the magnified inset. The lists can optionally be
#' named: `list(xmin = 1, xmax = 2, ymin = 3, ymax = 4)`.
#'
#' **Note**: very early versions of ggmagnify used a
#' different order of coordinates: `list(xmin, ymin, xmax, ymax)`.
#'
#' Alternatively, `from` can be:
#'
#' * A data frame of points with two columns for `x` and `y`, or a [grid::grob()]
#'   object. Points within the grob region (a polygon spanned by the data frame)
#'   will be magnified. Points should be
#'   on the same scale as the data, with `default.units = "native"` in the grob.
#'   `shape` will be ignored.
#'
#' * A logical vector. Points in the data where `from` is `TRUE` will be
#'   surrounded by a rectangle, ellipse or outline.
#'
#' Normally you'll set `from` and `to` in the call to `geom_magnify()`. You can
#' specify them as aesthetics, e.g. if you want different areas per facet. If
#' so, you may need to wrap them in a [list()] to make sure they are length one
#' per row of data. Only the first row per panel is used. (To magnify multiple
#' areas in one panel, use multiple calls to `geom_magnify()`.)
#'
#' ## Shapes
#'
#' If `shape = "ellipse"` an elliptical area is magnified. This may not include
#' all points within the target area given by `from`.
#'
#' If `shape = "outline"` then a convex hull will be drawn around points in
#' the target area. This only works if you are using [geom_point()] or some other
#' geom with aesthetics `x` and `y`. If you are plotting a map, then
#' `"outline"` magnifies exactly the map features selected by `from`.
#'
#' ## Projection lines
#'
#' * `proj = "corresponding"` or `"facing"` draws projection lines from the
#'   corners of the target to the corners of the inset.
#'   * `"corresponding"` always projects each corner of the target to the same
#'     corner of the inset.
#'   * `"facing"` sometimes draws lines between facing corners, when this looks
#'     cleaner.
#'   * For non-rectangular insets, `"facing"` and `"corresponding"` are the same.
#' * `"single"` draws a single line from the midpoint of facing sides.
#' * To draw no lines, set `proj.linetype = 0`.
#'
#' ## Limitations
#'
#' * `geom_magnify()` uses masks. This requires R version 4.1.0 or higher, and
#'   a graphics device that supports masking. If you are using knitr, you may have
#'   luck with the `ragg_png` device. If your device doesn't support masks,
#'   only `shape = "rect"` will work, and the plot inset will not be clipped
#'   to the panel area.
#'
#' * `geom_magnify()` uses dark magic to deal with faceting. It may break with
#'   older, or newer, versions of ggplot2. If you don't need faceting, and want
#'   your code to be robust to upgrades, set `options(ggmagnify.safe_mode = TRUE)`
#'   to use slightly less magic.
#'
#' * By design, `geom_magnify()` replots the original plot using new limits. It
#'   does not directly copy the target area pixels. The advantage is that you can
#'   e.g. add axes, plot points at an appropriate size, zoom in on data that's
#'   invisible in the main plot, or recompute derived graphics. If you want an
#'   exact pixel-by-pixel copy, use a different tool.
#'
#' * `geom_magnify()` may break with discrete scales. This is a limitation in
#'   ggplot2 for now.
#'
#' * Find a bug? Report it at <https://github.com/hughjonesd/ggmagnify/issues/>.
#'
#' @export
#'
#' @doctest
#' library(ggplot2)
#' ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
#'          geom_point() + xlim(c(2, 6))
#' from <- list(2.5, 3.5, 6, 7)
#' to <- list(4, 6, 5, 7)
#'
#' # Basic magnification
#' @expect silent()
#' ggp + geom_magnify(from = from, to = to)
#'
#' # Convex hull of points
#' @expect silent()
#' ggp + geom_magnify(aes(from = Species == "setosa"), to = c(3, 5, 6, 8),
#'                    shape = "outline")
#'
#' # Order matters
#'
#' # `geom_magnify()` stores the plot when it is added to it:
#' @expect silent()
#' ggp +
#'   scale_color_brewer() +
#'   geom_magnify(from = from, to = to)
#'
#' # This will print the inset without the new scale:
#' @expect no_error()
#' ggp +
#'   geom_magnify(from = from, to = to) +
#'   scale_color_brewer()
#'
#' # For more examples see https://github.com/hughjonesd/ggmagnify
#'
geom_magnify <- function (mapping = NULL,
                          data = NULL,
                          stat = StatMagnify,
                          position = "identity",
                          ...,
                          shape = c("rect", "ellipse", "outline"),
                          expand = 0.1,
                          aspect = c("free", "fixed"),
                          axes = "",
                          proj = c("facing", "corresponding", "single"),
                          shadow = FALSE,
                          corners = 0,
                          colour = "black",
                          linetype = 1,
                          target.linetype = linetype,
                          inset.linetype = linetype,
                          proj.linetype = 2,
                          alpha = 1,
                          linewidth = 0.4,
                          plot = NULL,
                          shadow.args = list(sigma = 5, colour = "grey40",
                                             x_offset = 5, y_offset = 5),
                          recompute = FALSE,
                          scale.inset = 1,
                          proj.combine = TRUE,
                          na.rm = FALSE,
                          inherit.aes = TRUE) {
  proj <- match.arg(proj)
  shape <- match.arg(shape)
  aspect <- match.arg(aspect)

  l <- layer_sf(
         geom = ggproto(NULL, GeomMagnify), # we clone because self$plot holds state
         mapping = mapping, data = data, stat = stat,
         position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
         params = list(expand = expand, aspect = aspect,
                       axes = axes, proj = proj, shadow = shadow,
                       corners = corners, colour = colour,
                       linewidth = linewidth, linetype = linetype, alpha = alpha,
                       target.linetype = target.linetype,
                       proj.linetype = proj.linetype,
                       inset.linetype = inset.linetype,
                       shape = shape, plot = plot,
                       shadow.args = shadow.args, recompute = recompute,
                       scale.inset = scale.inset, proj.combine = proj.combine,
                       na.rm = na.rm, ...)
       )
  class(l) <- c("GeomMagnifyLayer", class(l))

  l
}


#' @export
ggplot_add.GeomMagnifyLayer <- function(object, plot, object_name) {
  object$geom$plot <- plot
  NextMethod()
}


#' Internals
#'
#' @format NULL
#' @usage NULL
#' @export
GeomMagnify <- ggproto("GeomMagnify", Geom,
  required_aes = c("xmin","xmax", "ymin", "ymax", "to_xmin", "to_xmax",
                   "to_ymin", "to_ymax"),
  draw_key = draw_key_blank,
  plot = NULL,
  rename_size = FALSE,

  setup_params = function (data, params) {
    if (params$shadow) {
      rlang::check_installed("ggfx")
    }
    if (params$axes != "" && ! identical(params$shape, "rect")) {
      cli::cli_warn(paste("Setting {.code axes} to {.code \"\"} because",
                          "{.code shape} is not \"rect\"}"))
      params$axes <- ""
    }
    if (length(params$scale.inset) == 1L) {
      params$scale.inset <- rep(params$scale.inset, 2)
    }

    params
  },


  draw_panel = function (self, data, panel_params, coord, from,
                         magnify, axes, proj, shadow, corners, colour,
                         linetype, target.linetype, proj.linetype, inset.linetype,
                         linewidth, alpha, shape, expand, plot, shadow.args,
                         recompute, scale.inset, proj.combine
                         ) {
    # StatMagnify has put xmin, to_xmin and inset_xmin into this
    d1 <- data[1, , drop = FALSE]

    # create shape_grob for target border, inset border, and inset mask
    # this is scaled to 0, 1 in both directions, with
    # default units "npc", and has been transformed by the coords
    from <- if ("from" %in% names(data) && ! is.null(data$from[[1]])) {
              if (is.logical(data$from)) data$from else data$from[[1]]
            }  else {
              from
            }
    shape_grob <- compute_shape_grob(from, shape, corners, data, coord, panel_params,
                                     expand)

    # == create grob for border around target ==
    target_limits <- data.frame(
                       x = c(d1$xmin, d1$xmax),
                       y = c(d1$ymin, d1$ymax)
                     )

    target_limits_t <- coord$transform(target_limits, panel_params)
    target_x_rng <- range(target_limits_t$x, na.rm = TRUE)
    target_y_rng <- range(target_limits_t$y, na.rm = TRUE)

    target_vp <- viewport(x = mean(target_x_rng),
                          y = mean(target_y_rng),
                          width = diff(target_x_rng),
                          height = diff(target_y_rng),
                          default.units =  "native")
    target_grob <- editGrob(shape_grob, vp = target_vp,
                            gp = gpar(fill = NA, col = alpha(colour, alpha),
                                      lwd = linewidth * .pt,
                                      lty = target.linetype))

    # == create the magnified plot =======================================

    plot <- plot %||% self$plot
    plot_gtable <- create_plot_gtable(plot, data = d1, axes = axes,
                                      recompute = recompute,
                                      scale.inset = scale.inset)

    # == create the viewport and mask for the inset plot ==============

    limits <- data.frame(
      x = c(d1$to_xmin, d1$to_xmax),
      y = c(d1$to_ymin, d1$to_ymax)
    )
    limits_t <- coord$transform(limits, panel_params)
    x_rng <- range(limits_t$x, na.rm = TRUE)
    y_rng <- range(limits_t$y, na.rm = TRUE)
    # we use a mask here instead of clipping because gtable doesn't inherit
    # clip, and grid doesn't nest clips (so I guess ggplot needs its own
    # clipping, presumably when grid.draw is called on it?)
    mask_grob <- grid::editGrob(shape_grob,
                                gp = gpar(fill = rgb(0, 0, 0, 1)))
    # should be fine but needs testing
    # if (identical(shape, "rect")) mask_grob <- "inherit"
    mask_cap <- grDevices::dev.capabilities("mask")$masks
    if (isFALSE(mask_cap)) {
      cli::cli_warn(c("Device cannot use masks.",
                    "!" = "Only `shape = \"rect\"` will work correctly.",
                    "!" = "Inset will not be clipped to the plot area.",
                    "i" = "Try using a \"cairo\" graphics device."))
      mask_grob <- FALSE
    }
    # What if it's NA? Need to then check for length 1, since it may be
    # not NA, but length 2
    # This is the worst API :-P
    vp <- viewport(x = mean(x_rng), y = mean(y_rng), width = diff(x_rng),
                   height = diff(y_rng), default.units = "native",
                   mask = mask_grob)
    plot_gtable <- grid::editGrob(plot_gtable, vp = vp)

    # border_grob is already shaped like shape_grob; adding the mask
    # just cuts off its borders narrower
    border_vp <- grid::editViewport(vp, mask = "inherit")
    border_grob <- grid::editGrob(shape_grob, vp = border_vp,
                                  name = paste0("ggmagnify-border-",
                                                     incremental_id()),
                                  gp = gpar(fill = NA,
                                            col = alpha(colour, alpha),
                                            lwd = linewidth * .pt,
                                            lty = inset.linetype
                                            ))

    # == create projection lines =====
    proj_df <- if (identical(shape, "rect") && ! inherits(from, "grob") &&
                   ! inherits(from, "data.frame")) {
      calculate_proj_df_rect(proj, d1, corners, coord, panel_params)
    } else {
      calculate_proj_df(proj, proj.combine, target_grob, border_grob)
    }

    proj_grob <- segmentsGrob(proj_df$x, proj_df$y, proj_df$xend, proj_df$yend,
                              #vp = vp,
                              default.units = "native",
                              gp = gpar(
                                col = alpha(colour, alpha),
                                lty = proj.linetype,
                                lwd = linewidth * .pt
                              ))

    if (shadow) {
      plot_gtable <- do.call(ggfx::with_shadow, c(list(x = plot_gtable), shadow.args))
    }

    grid::gTree(name = paste0("ggmagnify-", incremental_id()),
          children = gList(target_grob, proj_grob, plot_gtable, border_grob))
  }
)


create_plot_gtable <- function (plot, data, axes, recompute, scale.inset) {
  plot_coord <- ggplot_build(plot)$layout$coord
  plot_limits <- plot_coord$limits

  if ("inset_xmin" %in% names(data)) {
    xlim_vals <- c(data$inset_xmin, data$inset_xmax)
    ylim_vals <- c(data$inset_ymin, data$inset_ymax)
  } else {
    xlim_vals <- c(data$xmin, data$xmax)
    ylim_vals <- c(data$ymin, data$ymax)
  }

  rev_x <- ! is.null(plot_limits$x) && diff(plot_limits$x) < 0
  rev_y <- ! is.null(plot_limits$y) && diff(plot_limits$y) < 0
  if (rev_x) xlim_vals <- xlim_vals[2:1]
  if (rev_y) ylim_vals <- ylim_vals[2:1]

  scale_lims <- function (lim, sc) {
    (lim - mean(lim)) * sc + mean(lim)
  }
  xlim_vals <- scale_lims(xlim_vals, scale.inset[1])
  ylim_vals <- scale_lims(ylim_vals, scale.inset[2])

  plot_coord <- constructor(plot_coord)
  suppressMessages(
    plot <- if (recompute) {
      plot + do.call(plot_coord, list(expand = FALSE)) +
        lims(x = xlim_vals, y = ylim_vals)
    } else {
      plot + do.call(plot_coord,
                     list(xlim = xlim_vals, ylim = ylim_vals, expand = FALSE))
    }
  )
  plot <- plot + inset_theme(axes = axes)

  suppressWarnings(suppressMessages({
    plot_built <- ggplot_build(plot)
  }))

  if (! getOption("ggmagnify.safe_mode", FALSE)) {
    panel_id <- as.numeric(data$PANEL[1])
    plot_built <- edit_to_panel(plot_built, panel_id)
  }

  suppressWarnings(suppressMessages(
    plot_gtable <- ggplot_gtable(plot_built)
  ))

  plot_gtable
}


edit_to_panel <- function (plot_built, panel_id) {
  # DARK MAGIC HERE ------------------------------------------------
  # the idea is to limit the plot to this particular panel
  # we edit the layout, and the plot data
  # obviously this messes deeply in ggplot2 internals

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

  plot_built
}
