
#' Deprecated
#'
#' `ggmagnify()` is deprecated. Use [geom_magnify()] instead.
#'
#' @param plot A ggplot object.
#' @param xlim,ylim Limits of the area to magnify. Length 2 numeric.
#' @param inset_xlim,inset_ylim Position of the inset in the main graph.
#'   Length 2 numeric.
#' @param zoom How much to magnify the inset by.
#' @param border Logical. Draw a border around the inset?
#' @param target Logical. Draw a border around the target area?
#' @param proj String. What style of projection lines to draw?
#' @param shadow Logical. Draw a shadow behind the inset?
#' @param axes Logical. Draw axes in the inset?
#' @param margin Plot margin of inset.
#' @param linewidth,linetype,colour,alpha Parameters for inset border, target border,
#'   and projection lines.
#' @param inset_linewidth,inset_linetype,inset_colour,inset_alpha Parameters
#'   for inset border.
#' @param proj_linewidth,proj_linetype,proj_colour,proj_alpha Parameters
#'   for projection lines.
#' @param target_linewidth,target_linetype,target_colour,target_alpha
#'  Parameters for target border.
#' @param shadow_args List of arguments to pass to [ggfx::with_shadow()].
#' @param blank Character vector of theme elements to blank out in the inset.
#' @param inset_expand Logical. Expand the inset's view of the target?
#' @param inset_coord Result of a call to a `ggplot2::coord_` function to use
#'   for the inset.
#'
#' @return
#' A `GgMagnify` object.
#' @export
#' @keywords internal
#' @doctest
#' library(ggplot2)
#'
#' ggp <- ggplot(ChickWeight, aes(Time, weight, group = Chick, color = Diet)) +
#'   geom_line()
#'
#' @expect silent()
#' ggmagnify(ggp,
#'           xlim = c(0, 5), ylim = c(30, 80),
#'           inset_xlim = c(0, 15), inset_ylim = c(200, 350))
#'
#'
ggmagnify <- function (
    plot,
    xlim,
    ylim,
    inset_xlim,
    inset_ylim,
    zoom,
    border = TRUE,
    target = TRUE,
    proj   = c("facing", "corresponding", "single", "none"),
    shadow = FALSE,
    axes = FALSE,
    margin = if (axes) 10 else 0,
    linewidth = 0.5,
    linetype = 1,
    colour = "black",
    alpha = 1,
    inset_linewidth = linewidth,
    inset_linetype = linetype,
    inset_colour = colour,
    inset_alpha = alpha,
    proj_linewidth = linewidth,
    proj_linetype = "dashed",
    proj_colour = colour,
    proj_alpha = alpha,
    target_linewidth = linewidth,
    target_linetype = linetype,
    target_colour = colour,
    target_alpha = alpha,
    shadow_args = list(sigma = 5, colour = "grey40", x_offset = 5, y_offset = 5),
    blank = inset_blanks(axes = axes),
    inset_expand = axes,
    inset_coord = ggplot2::coord_cartesian(xlim = xlim, ylim = ylim,
                                           expand = inset_expand)
) {
  xmin <- min(xlim)
  xmax <- max(xlim)
  ymin <- min(ylim)
  ymax <- max(ylim)

  inset_xmin <- min(inset_xlim)
  inset_xmax <- max(inset_xlim)
  inset_ymin <- min(inset_ylim)
  inset_ymax <- max(inset_ylim)

  if (! missing(zoom)) {
    if (length(zoom) < 2) zoom[2] <- zoom[1]
    inset_xmax <- inset_xmin + diff(xlim) * zoom[1]
    inset_ymax <- inset_ymin + diff(ylim) * zoom[2]
  }

  proj <- match.arg(proj)

  # == Create the inset ggplot =================================================

  suppressMessages({
    inset <- plot + inset_coord
  })

  blank_theme <- inset_theme(blank, axes = axes, margin = margin)

  inset <- inset + blank_theme

  # == Create the target border ================================================

  target <- if (! target) {
    NULL
  } else {
    ggplot2::annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin,
                       ymax = ymax, linetype = target_linetype,
                       linewidth = target_linewidth,
                       colour = ggplot2::alpha(target_colour, target_alpha),
                       fill = NA)
  }

  # == Create the inset border =================================================

  # not printing a border at all does weird things, so as a hack
  # we just print an invisible one
  border_colour <- if (! border) NA else ggplot2::alpha(inset_colour,
                                                        inset_alpha)
  border <- ggplot2::annotate("rect", xmin = inset_xmin, xmax = inset_xmax,
                              ymin = inset_ymin, ymax = inset_ymax,
                              linetype = inset_linetype,
                              linewidth = inset_linewidth,
                              colour = border_colour, fill = NA)

  # == Create projection lines =================================================
  if (proj == "none") {
    proj_layer <- NULL
  } else {
    proj_df <- calculate_proj_segments(proj, "rect", xmin, xmax, ymin, ymax,
                                    inset_xmin, inset_xmax, inset_ymin, inset_ymax)

    proj_layer <- ggplot2::annotate("segment",
                                    x = proj_df$x, y = proj_df$y,
                                    xend = proj_df$xend, yend = proj_df$yend,
                                    colour = proj_colour, alpha = proj_alpha,
                                    linewidth = proj_linewidth,
                                    linetype = proj_linetype)
  }

  # == Put the result together =================================================
  result <- list(
              plot = plot,
              inset = inset,
              border = border,
              target = target,
              proj = proj_layer,
              inset_xmin = inset_xmin,
              inset_xmax = inset_xmax,
              inset_ymin = inset_ymin,
              inset_ymax = inset_ymax,
              shadow = shadow,
              shadow_args = shadow_args
            )
  class(result) <- "GgMagnify"

  result
}


#' Deprecated.
#'
#' @param x A GgMagnify object.
#' @param ... Passed on to [ggplot2::print.ggplot()].
#'
#' @return The GgMagnify object, invisibly.
#' @export
print.GgMagnify <- function (x, ...) {
  print(compose(x)) # for some reason NextMethod didn't work here (but may now?)

  invisible(x)
}


#' @exportS3Method grid::grid.draw GgMagnify
grid.draw.GgMagnify <- function (x, recording = TRUE) {
  print(x)
}


compose <- function (x, plot = x$plot) {
  inset <- x$inset
  inset <- ggplot2::ggplotGrob(inset)
  shadow <- if (x$shadow) {
              shadow <- do.call(ggfx::with_shadow,
                                c(list(x = inset, stack = FALSE), x$shadow_args))
              ggplot2::annotation_custom(shadow, xmin = x$inset_xmin,
                                         xmax = x$inset_xmax,
                                         ymin = x$inset_ymin,
                                         ymax = x$inset_ymax)
            } else {
              NULL
            }
  inset <- ggplot2::annotation_custom(inset,
                                      xmin = x$inset_xmin, xmax = x$inset_xmax,
                                      ymin = x$inset_ymin, ymax = x$inset_ymax)

  plot + x$target + shadow + x$proj + inset + x$border
}
