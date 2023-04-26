

#' Default elements to blank in the ggmagnify inset
#'
#' @param ... Character vector of extra elements to blank.
#' @param axes Logical. `TRUE` if the inset will include axes.
#'
#' @return A character vector.
#' @export
inset_blanks <- function (..., axes) {
  res <- c("plot.title", "plot.subtitle", "plot.caption", "plot.tag",
              "axis.title", ...)
  blank_axes <- ! axes
  if (blank_axes) res <- c(res, "axis.text", "axis.ticks", "axis.line")

  res
}


#' Create a theme suitable for an inset ggplot
#'
#' @param blank Character vector of extra elements to blank.
#'   See [ggplot2::theme()].
#' @param axes Logical: will the inset have axes? Ignored if `blank` is
#'   set.
#'
#' @return A ggplot theme object
#' @export
inset_theme <- function (blank = inset_blanks(axes = axes), axes) {
  blank_elements <- lapply(blank, function (x) {
    ggplot2::element_blank()
  })
  names(blank_elements) <- blank
  blank_elements[["legend.position"]] <- "none"
  thm <- do.call(ggplot2::theme, blank_elements)

  thm
}

# TODO
# - maybe have an "expansion" factor which expands the target area from the
#   centre? A fudge to play with
# - think about specifying inset position. Cf. legend

#' Add a magnified inset plot to a ggplot object
#'
#' `ggmagnify()` magnifies a target area of your plot and adds it as an inset on
#' top of the original plot. Optional projection lines and borders around the
#' target are drawn. If the `ggfx` package is installed, a drop shadow can be
#' added.
#'
#' @param plot A ggplot object.
#' @param xlim,ylim Limits of the area to magnify. Length 2 numeric.
#' @param inset_xlim,inset_ylim Position of the inset in the main graph.
#'   Length 2 numeric.
#' @param zoom How much to magnify the inset by. Numeric. Specify two numbers
#'   for separate horizontal and vertical zoom. Optional: overrides `inset_xlim[2]`
#'   and `inset_ylim[2]`.
#' @param border Logical. Draw a border around the inset?
#' @param target Logical. Draw a border around the target area?
#' @param proj String. What style of projection lines to draw? `"facing"` (the
#'   default), `"corresponding"`, `"single"` or `"none"`. Can be abbreviated.
#'   See below.
#' @param shadow Logical. Draw a shadow behind the inset? Requires
#'   the `ggfx` package.
#' @param compose Logical. If `TRUE`, the new elements are added to the ggplot object.
#'   If `FALSE`, they are returned in a `GgMagnify` object, see below.
#' @param axes Logical. Draw axes in the inset?
#' @param margin Plot margin of inset. Can be a single number in "pt"
#'   units, a length 4 numeric (top, right, bottom, left), or a
#'   [ggplot2::margin()] object. Note that this is on the scale of the
#'   inset plot, not the outer plot.
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
#'   Use [`inset_blanks(elems, axes = axes)`][inset_blanks()] to add `elems`
#'   to the default list.
#'
#' @details
#'
#' ## Projection lines.
#'
#' `proj = "corresponding"` or `"facing"` draws projection lines from the
#' corners of the target to the corners of the inset. `"corresponding"` always
#' projects each corner of the target to the same corner of the inset.
#' `"facing"` sometimes draws lines between facing corners, when this looks
#' cleaner. `"single"` draws a single line from the midpoint of facing sides.
#' `"none"` draws no lines.
#'
#' If `compose` is `FALSE`, the returned `GgMagnify` object includes the
#' following list components:
#'
#' * `inset`, a ggplot object representing the inset.
#' * `border`, a layer representing the inset border.
#' * `target`, a layer representing the target border.
#' * `proj`, a layer representing the projection lines.
#'
#' You can modify these, e.g. by adding themes to the inset. Call
#' [`compose(ggm, plot)`][compose()] to add the object to the plot.
#'
#' To create an inset outside the plot area, set `coord_cartesian(clip = "off")`
#' in the main plot.
#'
#' ## Limitations
#'
#' It won't work with facets, or with non-cartesian coordinates.
#'
#' @return
#' The modified `plot` if `compose` is `TRUE`. Otherwise, a `GgMagnify`
#' object.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggp <- ggplot(diamonds, aes(carat, depth, color = cut)) + geom_point()
#'
#' ggmagnify(ggp,
#'           xlim = c(1.5, 2.5), ylim = c(60, 65),
#'           inset_xlim = c(2, 5), inset_ylim = c(40, 55))
#'
#' ggmagnify(ggp,
#'           xlim = c(1.5, 2.5), ylim = c(60, 65),
#'           inset_xlim = c(2, 5), inset_ylim = c(40, 55),
#'           colour = "red")
#'
#' ggmagnify(ggp,
#'           xlim = c(1.5, 2.5), ylim = c(60, 65),
#'           inset_xlim = c(2, 5), inset_ylim = c(40, 55),
#'           axes = TRUE, border = FALSE)
#'
#' if (requireNamespace("ggfx", quietly = TRUE)) {
#'   ggmagnify(ggp,
#'             xlim = c(1.5, 2.5), ylim = c(60, 65),
#'             inset_xlim = c(2, 5), inset_ylim = c(40, 55),
#'             shadow = TRUE)
#' }
#'
#' @example man/R/advanced-example.R
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
    compose = TRUE,
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
    blank = inset_blanks(axes = axes)
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

  suppressWarnings({
    inset <- plot + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim,
                                             expand = FALSE)
  })

  blank_theme <- inset_theme(blank, axes = NULL)

  if (! inherits(margin, "unit")) {
    if (length(margin) == 1) margin <- rep(margin, 4)
    margin <- grid::unit(margin, "pt")
  }

  inset <- inset +
           blank_theme +
           ggplot2::theme(plot.margin = margin)

  if (! axes) {
    inset <- inset + ggplot2::theme(axis.ticks.length = grid::unit(0, "pt"))
  }

  # == Create target border ====================================================
  target <- if (! target) {
    NULL
  } else {
    ggplot2::annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin,
                       ymax = ymax, linetype = target_linetype,
                       linewidth = target_linewidth,
                       colour = ggplot2::alpha(target_colour, target_alpha),
                       fill = NA)
  }

  # == Create inset border

  border <- if (! border) {
    NULL
  } else {
      ggplot2::annotate("rect", xmin = inset_xmin, xmax = inset_xmax,
                        ymin = inset_ymin, ymax = inset_ymax,
                        linetype = inset_linetype, linewidth = inset_linewidth,
                        colour = ggplot2::alpha(inset_colour, inset_alpha),
                        fill = NA)
  }

  # == Create projection lines =================================================
  if (proj == "none") {
    proj_layer <- NULL
  } else {
    if (proj %in% c("corresponding", "facing")) {
      # which of the four lines connecting the four corners can we draw?
      can_top_left  <- sign(xmin - inset_xmin) == sign(ymax - inset_ymax)
      can_bot_right <- sign(xmax - inset_xmax) == sign(ymin - inset_ymin)
      can_bot_left  <- sign(xmin - inset_xmin) != sign(ymin - inset_ymin)
      can_top_right <- sign(xmax - inset_xmax) != sign(ymax - inset_ymax)
      can_proj <- c(can_bot_left, can_top_left, can_bot_right, can_top_right)

      proj_x    <- c(xmin, xmin, xmax, xmax)
      proj_y    <- c(ymin, ymax, ymin, ymax)
      proj_xend <- c(inset_xmin, inset_xmin, inset_xmax, inset_xmax)
      proj_yend <- c(inset_ymin, inset_ymax, inset_ymin, inset_ymax)

      if (proj == "facing") {
        # If we can project on two adjacent corners, then we have the option
        # of joining corners to their "facing" rather than "corresponding" corner.
        # The "corresponding" corner looks a bit weird.
        # We only do this if there's no overlap (one min is bigger than other max)
        adjacent_horiz <- (can_top_left && can_top_right) ||
                          (can_bot_left && can_bot_right)
        adjacent_vert <-  (can_top_right && can_bot_right)  ||
                          (can_top_left  && can_bot_left)

        if (adjacent_horiz) {
          if (inset_ymin > ymax) {
            # "always project the top of the target to the bottom of the inset"
            proj_y <- rep(ymax, 4)
            proj_yend <- rep(inset_ymin, 4)
          } else if (inset_ymax < ymin) {
            proj_y <- rep(ymin, 4)
            proj_yend <- rep(inset_ymax, 4)
          }
        }
        if (adjacent_vert) {
          if (inset_xmin > xmax) {
            proj_x <- rep(xmax, 4)
            proj_xend <- rep(inset_xmin, 4)
          } else if (inset_xmax < xmin) {
            proj_x <- rep(xmin, 4)
            proj_xend <- rep(inset_xmax, 4)
          }
        }
      }

  } else if (proj == "single") {
    # t r b l midpoints of the target, opposite side of the inset:
    proj_x    <- c(mean(xlim), xmax, mean(xlim), xmin)
    proj_y    <- c(ymax, mean(ylim), ymin, mean(ylim))
    proj_xend <- c(mean(inset_xlim), inset_xmin, mean(inset_xlim), inset_xmax)
    proj_yend <- c(inset_ymin, mean(inset_ylim), inset_ymax, mean(inset_ylim))

    gaps <- c(inset_ymin - ymax, # top (of target, bottom of inset)
              inset_xmin - xmax, # right
              ymin - inset_ymax, # bottom
              xmin - inset_xmax) # left

    # We try to pick the "biggest" gap to make the line angle least acute.
    # We don't know the dimensions of the plot; we guess by using the
    # inset (theorizing it will have "sensible" dimensions). This is hacky...
    gaps <- gaps/c(diff(inset_ylim), diff(inset_xlim), diff(inset_ylim), diff(inset_xlim))
    can_proj <- which.max(gaps)
  }

    proj_x    <- proj_x[can_proj]
    proj_y    <- proj_y[can_proj]
    proj_xend <- proj_xend[can_proj]
    proj_yend <- proj_yend[can_proj]
    proj_layer <- ggplot2::annotate("segment", x = proj_x, y = proj_y, xend = proj_xend,
                                    yend = proj_yend, colour = proj_colour,
                                    alpha = proj_alpha, linewidth = proj_linewidth,
                                    linetype = proj_linetype)
  }

  # == Put the result together =================================================
  result <- list(
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

  if (compose) result <- compose(result, plot)

  result
}


#' Compose a GgMagnify object with its ggplot
#'
#' @param x A GgMagnify object.
#' @param plot A ggplot object.
#'
#' @return The modified plot.
#' @export
#'
#' @example man/R/advanced-example.R
compose <- function (x, plot) {
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


`%||%` <- function (x, y) {
    if (is.null(x)) y else x
}
