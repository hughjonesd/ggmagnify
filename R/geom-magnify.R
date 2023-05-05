
#' @import ggplot2
#' @import grid
NULL


# TODO:
# - make binned & discrete scales work (as and when ggplot2 faces reality...)
# - make it work with log scales et al... if you can, ha ha
# - why isn't geom_abline() working when recompute = FALSE?
#
# - if you have aes() at all, it makes sense to allow multiple on one plot
#   - but it's a very rare use case and overplotting will become a pain...


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
#'   `"ellipse` requires the "ggforce" package. Or (experimental!) pass in a
#'   [grid::grob()] for an arbitrary mask.
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
#' To magnify an arbitrary area, pass a grid grob into `shape`. The grob should
#' be scaled to between 0 and 1 on both dimensions. This is experimental. (Well,
#' everything is experimental. This is just *more* experimental.)
#'
#' ## Limitations
#'
#' * `geom_magnify()` uses masks. This requires R version 4.2.0 or higher, and
#'   a graphics device that supports masking. If you are using knitr, you may have
#'   luck with the `ragg_png` device.
#'
#'  * You can't set params `xmin,...` or `to_xmin,...` directly in the call
#'    to `geom_magnify()`. If you want to set them, supply `data` and `mapping`
#'    arguments.
#'
#' * `geom_magnify()` uses dark magic to deal with faceting. It may break with
#'   older (or newer!) versions of ggplot2.
#'
#' * By design, `geom_magnify()` replots the original plot using new limits. It
#'   does not directly copy the target area pixels. The advantage is that you can
#'   e.g. add axes, plot points at an appropriate size, or recompute derived
#'   graphics.
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
#' if (requireNamespace("ggforce", quietly = TRUE) && getRversion() >= 4.2) {
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
#' shape <- grid::polygonGrob()
#' @expect no_error()
#' ggp + geom_magnify(from = c(3, 6.5, 4, 7.5),
#'                    to = c(4, 5, 7, 6.5),
#'                    shape = mask)
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
    if (is.character(params$shape)) {
      params$shape <- match.arg(params$shape, c("rect", "ellipse"))
    }
    if (identical(params$shape, "ellipse")) {
      rlang::check_installed("ggforce")
    }
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

    params
  },

  draw_panel = function (self, data, panel_params, coord, from, to,
                         magnify, axes, proj, shadow,
                         linetype, target.linetype, proj.linetype, inset.linetype,
                         linewidth, alpha, shape, expand, plot, shadow.args,
                         recompute, scale.inset
                         ) {
    d1 <- data[1, , drop = FALSE]   # untransformed, for other geoms

    # == create grob for border around inset ==
    target_df <- data.frame(xmin = d1$xmin, xmax = d1$xmax, ymin = d1$ymin,
                            ymax = d1$ymax,
                            colour = alpha(d1$colour, alpha), fill = NA,
                            linewidth = linewidth, alpha = NA,
                            linetype = target.linetype,
                            group = 1L) # group = 1 needed for some coord systems
    target_grob <- if (identical(shape, "rect")) {
                     GeomRect$draw_panel(target_df, panel_params, coord)
                   } else if (identical(shape, "ellipse")) {
                     make_geom_ellipse_grob(target_df, panel_params, coord)
                   } else {
                     target_corners <- data.frame(
                       x = c(d1$xmin, d1$xmax),
                       y = c(d1$ymin, d1$ymax)
                     )
                     target_corners_t <- coord$transform(target_corners, panel_params)
                     target_x_rng <- range(target_corners_t$x, na.rm = TRUE)
                     target_y_rng <- range(target_corners_t$y, na.rm = TRUE)
                     target_vp <- viewport(x = mean(target_x_rng),
                                           y = mean(target_y_rng),
                                           width = diff(target_y_rng),
                                           height = diff(target_y_rng))
                     editGrob(shape,
                              vp = target_vp,
                              gp = gpar(fill = NA, col = alpha(d1$colour, alpha),
                                            lwd = linewidth * .pt,
                                            lty = target.linetype
                                            ))
                   }

    # == draw projection lines ==========================================

    proj_df <- calculate_proj_segments(
                 proj = proj, shape = shape,
                 xmin = d1$xmin, xmax = d1$xmax,
                 ymin = d1$ymin, ymax = d1$ymax,
                 to_xmin = d1$to_xmin, to_xmax = d1$to_xmax,
                 to_ymin = d1$to_ymin, to_ymax = d1$to_ymax
               )
    proj_grob <- if (is.null(proj_df) || nrow(proj_df) == 0L) NULL else {
      proj_df$colour <- d1$colour
      proj_df$alpha <- alpha
      proj_df$linewidth <- linewidth
      proj_df$linetype <- proj.linetype
      GeomSegment$draw_panel(proj_df, panel_params, coord)
    }

    # == draw the magnified plot ========================================
    plot <- plot %||% self$plot
    # or just coord, unless `plot` might have a different coord
    plot_coord <- ggplot_build(plot)$layout$coord
    plot_limits <- plot_coord$limits

    rev_x <- ! is.null(plot_limits$x) && diff(plot_limits$x) < 0
    rev_y <- ! is.null(plot_limits$y) && diff(plot_limits$y) < 0

    if ("inset_xmin" %in% names(d1)) {
      xlim_vals <- c(d1$inset_xmin, d1$inset_xmax)
      ylim_vals <- c(d1$inset_ymin, d1$inset_ymax)
    } else {
      xlim_vals <- c(d1$xmin, d1$xmax)
      ylim_vals <- c(d1$ymin, d1$ymax)
    }
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

    suppressWarnings(suppressMessages(
      plot_gtable <- ggplot_gtable(plot_built)
    ))

    # == create the viewport and mask for the inset plot ==============

    corners <- data.frame(
      x = c(d1$to_xmin, d1$to_xmax),
      y = c(d1$to_ymin, d1$to_ymax)
    )
    corners_t <- coord$transform(corners, panel_params)

    x_rng <- range(corners_t$x, na.rm = TRUE)
    y_rng <- range(corners_t$y, na.rm = TRUE)
    mask <- if (identical(shape, "ellipse")) {
      ellipse_df <- data.frame(xmin = 0, xmax = 1,
                               ymin = 0, ymax = 1)
      el_pts <- ellipse_points(ellipse_df)
      grid::polygonGrob(x = el_pts$x, y = el_pts$y,
                        default.units = "native",
                        gp = gpar(fill = rgb(0,0,0,1)))
    } else if (identical(shape, "rect")) {
      grid::rectGrob(default.units = "native",
                     gp = gpar(fill = rgb(0,0,0,1)))
    } else {
      grid::editGrob(shape, gp = gpar(fill = rgb(0, 0, 0, 1)))
    }

    border_grob <- grid::editGrob(mask,
                                  name = paste0("ggmagnify-border-",
                                                     incremental_id()),
                                  gp = gpar(fill = NA,
                                            col = alpha(d1$colour, alpha),
                                            lwd = linewidth * .pt,
                                            lty = inset.linetype
                                            ))
    # browser()
    # we use a mask here instead of clipping because gtable doesn't inherit
    # clip, and grid doesn't nest clips (so I guess ggplot needs its own
    # clipping, presumably when grid.draw is called on it?)
    vp <- viewport(x = mean(x_rng), y = mean(y_rng), width = diff(x_rng),
                   height = diff(y_rng), default.units = "native",
                   mask = mask)
    plot_gtable <- grid::editGrob(plot_gtable, vp = vp)
    border_grob <- grid::editGrob(border_grob, vp = vp)

    if (shadow) {
      plot_gtable <- do.call(ggfx::with_shadow, c(list(x = plot_gtable), shadow.args))
    }

    grid::gTree(name = paste0("ggmagnify-", incremental_id()),
          children = gList(target_grob, proj_grob, plot_gtable, border_grob))
  }
)


make_geom_ellipse_grob <- function (df, panel_params, coord) {
  el_pts <- ellipse_points(df)
  ggforce::GeomCircle$draw_panel(el_pts, panel_params, coord)
}


ellipse_points <- function(df) {
  df$xmin <- as.numeric(df$xmin)
  df$xmax <- as.numeric(df$xmax)
  df$ymin <- as.numeric(df$ymin)
  df$ymax <- as.numeric(df$ymax)
  df$x0 = (df$xmin + df$xmax)/2
  df$y0 = (df$ymin + df$ymax)/2
  df$a =  (df$xmax - df$xmin)/2
  df$b =  (df$ymax - df$ymin)/2
  df$angle = 0
  df$group = 1
  ellipse_df <- ggforce::StatEllip$setup_data(df)
  el_pts <- ggforce::StatEllip$compute_panel(data = ellipse_df, scales = NULL)

  el_pts
}


#' @export
ggplot_add.GeomMagnifyLayer <- function(object, plot, object_name) {
  object$geom$plot <- plot
  NextMethod()
}


StatMagnify <- ggproto("StatMagnify", Stat,
  optional_aes = c("xmin", "xmax", "ymin", "ymax",
                   "to_xmin", "to_xmax", "to_ymin", "to_ymax"),

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
