
#' @import ggplot2
#' @import grid
NULL


# TODO:
# - make binned & discrete scales work (as and when ggplot2 faces reality...)
# - make it work with log scales et al... if you can, ha ha
# - why isn't geom_abline() working when recompute = FALSE?
# - separate inset_xlim from to_xlim etc.; same by default
#   - sometimes when you recompute, you want to use different limits, esp.
#     y limits, in the inset
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
#'   `"ellipse` requires the "ggforce" package.
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
#' `from` and `to`, but you can use the aesthetics to specify the magnification
#' area within your data, e.g. by facet. `from` and `to` override `xlim` etc.
#' and `to_xlim` etc. respectively.
#' *Note:* as of today, the code only allows one magnification per panel.
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
#' ## Limitations
#'
#' `geom_magnify()` uses masks. This requires R version 4.2.0 or higher, and
#' a graphics device that supports masking. If you are using knitr, you may have
#' luck with the `ragg_png` device.
#'
#' `geom_magnify()` uses dark magic to deal with faceting. It may break with
#' older (or newer!) versions of ggplot2.
#'
#' By design, `geom_magnify()` replots the original plot using new limits. It
#' does not directly copy the target area pixels. The advantage is that you can
#' e.g. add axes, plot points at an appropriate size, or recompute derived
#' graphics.
#'
#' ## TODO:
#'
#' * Support for non-standard scales
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
geom_magnify <- function (mapping = NULL, data = NULL,  stat = "identity",
                           position = "identity", ...,
                           from,
                           to,
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
  # this just causes pain
  # required_aes = c("xmin", "xmax", "ymin", "ymax",
  #                  "to_xmin", "to_xmax", "to_ymin", "to_ymax"),
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
    if (params$shadow) {
      rlang::check_installed("ggfx")
    }
    if (params$axes != "" && params$shape == "ellipse") {
      cli::cli_warn(paste("Setting {.code axes} to {.code \"\"} with",
                          "{.code shape = \"ellipse\"}"))
      params$axes <- ""
    }
    if (length(params$scale.inset) == 1L) {
      params$scale.inset <- rep(params$scale.inset, 2)
    }

    params
  },

  setup_data = function (data, params) {
    if (! is.null(params$from)) {
      from <- params$from
      data$xmin <- from[1]
      data$ymin <- from[2]
      data$xmax <- from[3]
      data$ymax <- from[4]
    }
    if (! is.null(params$to)) {
      to <- params$to
      data$to_xmin <- to[1]
      data$to_ymin <- to[2]
      data$to_xmax <- to[3]
      data$to_ymax <- to[4]
    }

    data
  },

  draw_panel = function (self, data, panel_params, coord, from, to,
                         magnify, axes, proj, shadow,
                         linetype, target.linetype, proj.linetype, inset.linetype,
                         linewidth, alpha, shape, expand, plot, shadow.args,
                         recompute, scale.inset
                         ) {
    d1 <- data[1, , drop = FALSE]   # untransformed, for other geoms

    # == draw borders around target and inset ==
    target_df <- data.frame(xmin = d1$xmin, xmax = d1$xmax, ymin = d1$ymin,
                            ymax = d1$ymax,
                            colour = alpha(d1$colour, alpha), fill = NA,
                            linewidth = linewidth, alpha = NA,
                            linetype = target.linetype,
                            group = 1L) # group = 1 needed for some coord systems
    target_grob <- if (shape == "rect") {
                     GeomRect$draw_panel(target_df, panel_params, coord)
                   } else {
                     make_geom_ellipse_grob(target_df, panel_params, coord)
                   }

    border_df <- data.frame(xmin = d1$to_xmin, xmax = d1$to_xmax, ymin = d1$to_ymin,
                            ymax = d1$to_ymax,
                            colour = alpha(d1$colour, alpha), fill = NA,
                            linewidth = linewidth, alpha = NA,
                            linetype = inset.linetype,
                            group = 1L)
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
    xlim_vals <- c(d1$xmin, d1$xmax)
    ylim_vals <- c(d1$ymin, d1$ymax)
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
      plot_gtable <- do.call(ggfx::with_shadow, c(list(x = plot_gtable), shadow.args))
    }

    grid::gTree(name = paste("ggmagnify", annotation_id()),
          children = gList(target_grob, proj_grob, plot_gtable, border_grob))
  },
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
ggplot_add.GeomMagnifyLayer <- function(object, plot, object_name) {
  object$geom$plot <- plot
  NextMethod()
}

# The below are derived from ggplot2 code

annotation_id <- local({
  i <- 1
  function() {
    i <<- i + 1
    i
  }
})


constructor <- function (x) {
  # copypasted from ggplot2 snakeize()
  x <- class(x)[1]
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  x <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", x)
}


plot_clone <- function (plot) {
    p <- plot
    p$scales <- plot$scales$clone()
    p
}

`%||%` <- function (x, y) {
    if (is.null(x)) y else x
}
