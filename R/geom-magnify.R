
#' @import ggplot2
#' @import grid
NULL


# TODO:
# - make binned & discrete scales work (as and when ggplot2 faces reality...)
# - why isn't geom_abline() working when recompute = FALSE?
#
#  - `fixed` aspect ratio, useful with grobs.
#
# - remove R > 4.1 dependency unless shape != "rect", i.e. get rid of the
#   meaningless mask
#
# NOT TODO
# - if you have aes() at all, it makes sense to allow multiple on one plot
#   - but it's a very rare use case and overplotting will become a pain...
#   - and it's liable to blow up when you put your aesthetics in the data.
#     Not worth it


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
#' @param from Length 4 numeric: points `x0, y0, x1, y1` of the target area to magnify.
#'   Alternatively, pass in a data frame or [grid::grob()] object, see below.
#' @param to Length 4 numeric: points `x0, y0, x1, y1` of the magnified inset.
#' @param expand Logical. Expand the limits of the target area by a small factor
#'   in the inset plot. See [coord_cartesian()][ggplot2::coord_cartesian()].
#' @param axes String. Which axes to plot in the inset? `""`, `"x"`, `"y"` or
#'   `"xy"`.
#' @param proj String. What style of projection lines to draw? `"facing"` (the
#'   default), `"corresponding"` or `"single"`. Can be abbreviated. See below.
#' @param shadow Logical. Draw a shadow behind the inset plot? Requires the
#'   "ggfx" package.
#' @param linetype,alpha,linewidth Linetype, alpha and linewidth for borders
#'   and projection lines. Set `linetype = 0` for no lines.
#' @param target.linetype,inset.linetype,proj.linetype Linetypes
#'   for specific components.
#' @param shape `"rect"` to magnify a rectangle. `"ellipse"` to magnify an ellipse.
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
#'
#' @details
#' ## Aesthetics
#'
#' geom_magnify understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **xmin**
#' - **xmax**
#' - **ymin**
#' - **ymax**
#' - **to_xmin**
#' - **to_xmax**
#' - **to_ymin**
#' - **to_ymax**
#' - colour
#'
#' Normally you'll set these in the call to `geom_magnify()`, by specifying
#' `from` and `to`, but you can set them directly if you prefer to be explicit.
#' (`from` and `to` override `xmin` etc. and `to_xmin` etc. respectively.) Or
#' use the aesthetics to specify the magnification area within your data, e.g.
#' by facet.
#'
#' *Note:* as of today, the code only allows one magnification per call to
#' `geom_magnify()`. However, nothing stops you adding multiple calls to
#' `geom_magnify()`
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
#' ## Arbitrary shapes
#'
#' To magnify an arbitrary area, pass a grid grob or a data frame of x and y
#' points into `from`. Points should be on the same scale as the data,
#' with `default.units = "native"`. This is experimental. (Well, everything is
#' experimental. This is just *more* experimental.)
#'
#' ## Limitations
#'
#' * `geom_magnify()` uses masks. This requires R version 4.1.0 or higher, and
#'   a graphics device that supports masking. If you are using knitr, you may have
#'   luck with the `ragg_png` device.
#'
#' * `geom_magnify()` uses dark magic to deal with faceting. It may break with
#'   older (or newer!) versions of ggplot2.
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
#' ggp + geom_magnify(from = c(3, 6.5, 4, 7.5),
#'                      to = c(4, 5, 7, 6.5))
#'
#' # Inset axes
#' ggp + geom_magnify(from = c(3, 6.5, 4, 7.5),
#'                      to = c(4, 5, 7, 6.5), axes = TRUE)
#'
#' # Ellipse magnification
#' @expect silent()
#' if (getRversion() >= 4.2) {
#'   ggp + geom_magnify(from = c(3, 6.5, 4, 7.5),
#'                      to = c(4, 5, 7, 6.5), shape = "ellipse")
#' }
#'
#' # Shadow
#' @expect silent()
#' if (requireNamespace("ggfx", quietly = TRUE)) {
#'   ggp + geom_magnify(from = c(3, 6.5, 4, 7.5),
#'                      to = c(4, 5, 7, 6.5), shadow = TRUE)
#' }
#'
#' # Arbitrary shape using grid
#' setosas <- iris[iris$Species == "setosa", ]
#' setosa_hull <- grDevices::chull(setosas[, c("Sepal.Width", "Sepal.Length")])
#' setosa_hull <- setosas[setosa_hull, c("Sepal.Width", "Sepal.Length")]
#'
#' @expect no_error()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) +
#'        geom_point() + xlim(c(2, 5)) +
#'        geom_magnify(from = setosa_hull, to = c(3, 6, 5, 8))
#'
#' # Order matters
#'
#' # `geom_magnify()` stores the plot when it is added to it:
#' @expect no_error()
#' ggp +
#'   geom_smooth() +
#'   geom_magnify(from = c(3, 6.5, 4, 7.5),
#'                to = c(4, 5, 7, 6.5))
#'
#' # This will print the inset without the smooth:
#' @expect no_error()
#' ggp +
#'   geom_magnify(from = c(3, 6.5, 4, 7.5),
#'                to = c(4, 5, 7, 6.5)) +
#'   geom_smooth()
#'
geom_magnify <- function (mapping = NULL, data = NULL, stat = StatMagnify,
                           position = "identity", ...,
                           from = NULL,
                           to = NULL,
                           expand = TRUE,
                           axes = "",
                           proj = c("facing", "corresponding", "single"),
                           shadow = FALSE,
                           linetype = 1,
                           target.linetype = linetype,
                           inset.linetype = linetype,
                           proj.linetype = 2,
                           alpha = 1,
                           linewidth = 0.4,
                           shape = c("rect", "ellipse"),
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
         params = list(from = from, to = to,
                       expand = expand, axes = axes, proj = proj, shadow = shadow,
                       linewidth = linewidth, linetype = linetype, alpha = alpha,
                       target.linetype = target.linetype,
                       proj.linetype = proj.linetype,
                       inset.linetype = inset.linetype,
                       shape = shape, plot = plot,
                       shadow.args = shadow.args, recompute = recompute,
                       scale.inset = scale.inset,
                       na.rm = na.rm,...)
       )
  class(l) <- c("GeomMagnifyLayer", class(l))

  l
}


#' Internals
#'
#' @format NULL
#' @usage NULL
#' @export
GeomMagnify <- ggproto("GeomMagnify", Geom,
  optional_aes = c("xmin", "xmax", "ymin", "ymax",
                   "to_xmin", "to_xmax", "to_ymin", "to_ymax"),
  default_aes = aes(colour = "black"),
  draw_key = draw_key_blank,
  plot = NULL,
  rename_size = FALSE,

  setup_params = function (data, params) {
    params$proj <- match.arg(params$proj, c("facing", "corresponding", "single"))
    params$shape <- match.arg(params$shape, c("rect", "ellipse"))

    if (params$shadow) {
      rlang::check_installed("ggfx")
    }
    if (params$axes != "" && ! identical(params$shape, "rect")) {
      cli::cli_warn(paste("Setting {.code axes} to {.code \"\"} with",
                          "{.code shape != \"rect\"}"))
      params$axes <- ""
    }
    if (length(params$scale.inset) == 1L) {
      params$scale.inset <- rep(params$scale.inset, 2)
    }
    if (is.data.frame(params$from)) {
      # we've got the xmin et al. from the grob in StatMagnify
      params$from <- df_to_grob(params$from)
    }

    params
  },

  draw_panel = function (self, data, panel_params, coord, from = NULL, to,
                         magnify, axes, proj, shadow,
                         linetype, target.linetype, proj.linetype, inset.linetype,
                         linewidth, alpha, shape, expand, plot, shadow.args,
                         recompute, scale.inset
                         ) {
    d1 <- data[1, , drop = FALSE]   # untransformed, for other geoms
    if (grid::is.grob(from)) {
      # this will be on scale of data
      shape <- from
      shape_cc <- as.data.frame(allcoords(shape))
      shape_cc <- coord$transform(shape_cc, panel_params)
      # this is rough, could we do better by rescaling?
      shape <- polygonGrob(x = shape_cc$x, y = shape_cc$y,
                        default.units = "native")
    }
    # create shape_grob for target border, inset border, and inset mask
    # this is scaled to 0, 1 in both directions, with
    # default units "npc"
    shape_grob <- if (grid::is.grob(shape)) {
      scale01 <- function (x) (x - min(x))/(max(x) - min(x))
      x_scaled <- scale01(as.numeric(shape$x))
      y_scaled <- scale01(as.numeric(shape$y))
      editGrob(shape, x = grid::unit(x_scaled, "npc"),
                      y = grid::unit(y_scaled, "npc"))
    } else if (shape == "ellipse"){
      gridExtra::ellipseGrob(x = 0.5, y = 0.5, size = 0.5,
                             position.units = "npc", size.units = "npc")
    } else {
      grid::rectGrob()
    }

    # == create grob for border around target ==
    target_corners <- data.frame(
                       x = c(d1$xmin, d1$xmax),
                       y = c(d1$ymin, d1$ymax)
                     )

    target_corners_t <- coord$transform(target_corners, panel_params)
    target_x_rng <- range(target_corners_t$x, na.rm = TRUE)
    target_y_rng <- range(target_corners_t$y, na.rm = TRUE)

    target_vp <- viewport(x = mean(target_x_rng),
                          y = mean(target_y_rng),
                          width = diff(target_x_rng),
                          height = diff(target_y_rng),
                          default.units =  "native")
    target_grob <- editGrob(shape_grob, vp = target_vp,
                            gp = gpar(fill = NA, col = alpha(d1$colour, alpha),
                                      lwd = linewidth * .pt,
                                      lty = target.linetype))

    # == create the magnified plot =======================================

    plot <- plot %||% self$plot
    plot_gtable <- create_plot_gtable(plot, data = d1, expand = expand,
                                      axes = axes, recompute = recompute,
                                      scale.inset = scale.inset)

    # == create the viewport and mask for the inset plot ==============

    corners <- data.frame(
      x = c(d1$to_xmin, d1$to_xmax),
      y = c(d1$to_ymin, d1$to_ymax)
    )
    corners_t <- coord$transform(corners, panel_params)
    x_rng <- range(corners_t$x, na.rm = TRUE)
    y_rng <- range(corners_t$y, na.rm = TRUE)
    # we use a mask here instead of clipping because gtable doesn't inherit
    # clip, and grid doesn't nest clips (so I guess ggplot needs its own
    # clipping, presumably when grid.draw is called on it?)
    mask_grob <- grid::editGrob(shape_grob,
                                gp = gpar(fill = rgb(0, 0, 0, 1)))
    # should be fine but needs testing
    # if (identical(shape, "rect")) mask_grob <- "inherit"
    vp <- viewport(x = mean(x_rng), y = mean(y_rng), width = diff(x_rng),
                   height = diff(y_rng), default.units = "native",
                   mask = mask_grob)
    plot_gtable <- grid::editGrob(plot_gtable, vp = vp)
    border_grob <- grid::editGrob(shape_grob, vp = vp,
                                  name = paste0("ggmagnify-border-",
                                                     incremental_id()),
                                  gp = gpar(fill = NA,
                                            col = alpha(d1$colour, alpha),
                                            lwd = linewidth * .pt,
                                            lty = inset.linetype
                                            ))

    # == create projection lines =====
    proj_df <- if (identical(shape, "rect")) {
      calculate_proj_df_rect(proj, d1, coord, panel_params)
    } else {
      calculate_proj_df(proj, target_grob, border_grob)
    }

    proj_grob <- segmentsGrob(proj_df$x, proj_df$y, proj_df$xend, proj_df$yend,
                              #vp = vp,
                              default.units = "native",
                              gp = gpar(
                                col = alpha(d1$colour, alpha),
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


create_plot_gtable <- function (plot, data, expand, axes, recompute, scale.inset) {
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
      plot + do.call(plot_coord, list(expand = expand)) +
        lims(x = xlim_vals, y = ylim_vals)
    } else {
      plot + do.call(plot_coord,
                     list(xlim = xlim_vals, ylim = ylim_vals, expand = expand))
    }
  )
  plot <- plot + inset_theme(axes = axes)

  suppressWarnings(suppressMessages({
    plot_built <- ggplot_build(plot)
  }))

  panel_id <- as.numeric(data$PANEL[1])
  plot_built <- edit_to_panel(plot_built, panel_id)

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


#' @export
ggplot_add.GeomMagnifyLayer <- function(object, plot, object_name) {
  object$geom$plot <- plot
  NextMethod()
}


StatMagnify <- ggproto("StatMagnify", Stat,
  optional_aes = c("xmin", "xmax", "ymin", "ymax",
                   "to_xmin", "to_xmax", "to_ymin", "to_ymax"),

  setup_params = function (self, data, params) {
    if (! is.null(params$from)) {
      if (is.data.frame(params$from)) {
        params$from <- df_to_grob(params$from)
      }
      if (is.grob(params$from)) {
        from_grob <- params$from
        grobcc <- allcoords(from_grob)
        params$from <- c(min(grobcc[,"x"]), min(grobcc[,"y"]), max(grobcc[,"x"]),
                         max(grobcc[,"y"]))
      }
    }

    params
  },

  setup_data = function (self, data, params, scales) {
    for (var in self$optional_aes) {
      if (var %in% names(params)) data[[var]] <- params[[var]]
    }

    data
  },

  # note: these parameters do magic by computing Stat$parameters()
  compute_group = function (data, scales, from = NULL, to = NULL,
                            xmin = NULL, ymin = NULL, xmax = NULL, ymax = NULL,
                            to_xmin = NULL, to_ymin = NULL, to_xmax = NULL,
                            to_ymax = NULL) {
    if (! is.null(from)) {
      data$xmin <- from[1]
      data$ymin <- from[2]
      data$xmax <- from[3]
      data$ymax <- from[4]
    }

    data[c("inset_xmin", "inset_ymin", "inset_xmax", "inset_ymax")] <-
      data[c("xmin", "ymin", "xmax", "ymax")]

    transform_x <- ! is.null(scales$x) && ! scales$x$is_discrete()
    transform_y <- ! is.null(scales$y) && ! scales$y$is_discrete()

    # if from is not null then xmin and friends will already have been
    # through the transformation
    if (! is.null(from)) {
      if (transform_x) {
        data[, c("xmin", "xmax")] <- scales$x$transform_df(data[, c("xmin", "xmax")])
      }
      if (transform_y) {
        data[, c("ymin", "ymax")] <- scales$y$transform_df(data[, c("ymin", "ymax")])
      }
    }

    if (! is.null(to)) {
      data$to_xmin <- to[1]
      data$to_ymin <- to[2]
      data$to_xmax <- to[3]
      data$to_ymax <- to[4]
      if (transform_x) {
        data$to_xmin <- scales$x$transform(data$to_xmin)
        data$to_xmax <- scales$x$transform(data$to_xmax)
      }
      if (transform_y) {
        data$to_ymin <- scales$y$transform(data$to_ymin)
        data$to_ymax <- scales$y$transform(data$to_ymax)
      }
    }

    data
  }
)

df_to_grob <- function (df) {
  # the [[]] matters so we get vectors
  grid::polygonGrob(x = df[[1]], y = df[[2]], default.units = "native")
}
