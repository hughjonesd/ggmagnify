
#' Compute a shape grob on scale 0-1 npc for use for borders,
#' target lines and mask
#'
#' @param from Stripped of surrounding `list()`.
#' @param shape "rect", "ellipse", "outline".
#' @param corners Numeric. Radius of corners for "rect" shape.
#' @param data Data.
#' @param coord Coord object.
#' @param panel_params Opaque object.
#' @param expand Parameter for proportional expansion (not done yet).
#'
#' @return A [grid::grob()] object with units "npc" and data between 0 and
#' 1 on screen scale. Coordinates have been transformed by `coord$transform(...)`.
#' @noRd
compute_shape_grob <- function (from, shape, corners, data, coord,
                                panel_params, expand) {
  UseMethod("compute_shape_grob")
  # if from is a grob, use it directly (after rescaling). If from is a data
  # frame, make a grob
  # and use that.
  # Otherwise: if shape is rect or ellipse, then return those.
  # If shape is outline then select points - a convex hull or sf grob
}


compute_shape_grob.grob <- function (from, shape, corners, data, coord,
                                     panel_params, expand) {
  scale01 <- function (x) (x - min(x))/(max(x) - min(x))
  scalexy <- function(mx) {
    # we don't transform for a raw grob; and we've expanded the bounding box
    # already
    x_scaled <- scale01(as.numeric(mx[, "x"]))
    y_scaled <- scale01(as.numeric(mx[, "y"]))
    cbind(x = x_scaled, y = y_scaled)
  }

  from_list_cc <- allcoords(from, bind = FALSE) # list of matrices
  lens <- vapply(from_list_cc, nrow, numeric(1L))
  from_cc <- do.call(rbind, from_list_cc)
  from_cc <- scalexy(from_cc)
  grid::polygonGrob(x = grid::unit(from_cc[, "x"], "npc"),
                    y = grid::unit(from_cc[, "y"], "npc"), id.lengths = lens)
}


compute_shape_grob.data.frame <- function (from, shape, corners, data, coord,
                                           panel_params, expand) {
  # this will be on scale of data
  names(from) <- c("x", "y")
  from_grob <- polygonGrob(x = from$x, y = from$y,
                            default.units = "native")
  compute_shape_grob(from_grob, shape, corners, data, coord, panel_params, expand)
}


compute_shape_grob.default <- function (from, shape, corners, data, coord,
                                        panel_params, expand) {
  if (shape == "rect") {
    # grid::rectGrob()
    ggforce::shapeGrob(x = c(0, 0, 1, 1),
                       y = c(0, 1, 1, 0), radius = corners)
  } else if (shape == "ellipse") {
    # resist the temptation to replace this with cirleGrob. You need to
    # mess with it later.
    gridExtra::ellipseGrob(x = 0.5, y = 0.5, size = 0.5, n = 180,
                             position.units = "npc", size.units = "npc")
  } else {
    data <- subset_by_from(from, data)
    # shape is "outline"
    # we have to select points inside the data
    # then build the hull or call geometry
    if (! is.null(data$geometry) && inherits(data$geometry, "sfc")) {
      rlang::check_installed("sf")
      from <- sf::st_as_grob(data$geometry)
      compute_shape_grob(from, shape, corners, data, coord, panel_params, expand)
    } else if ("x" %in% names(data) && "y" %in% names(data)){
      # only here we use the actual x and y points
      from_df <- hull_around(data$x, data$y, expand = 0)
      from_df <- coord$transform(from_df, panel_params)

      from_grob <- polygonGrob(x = from_df$x, y = from_df$y,
                               default.units = "native")
      compute_shape_grob(from_grob, shape, corners, data, coord, panel_params,
                         expand = 0)
    } else {
      cli::cli_warn(c("Couldn't find `x` and `y` to build convex hull.",
                      "Falling back to shape = \"rect\""))
      compute_shape_grob(from_grob, shape = "rect", corners, data, coord,
                         panel_params, expand = 0)
    }
  }
}


subset_by_from <- function (from, data) {
  UseMethod("subset_by_from")
}


subset_by_from.logical <- function (from, data) {
  data[from,]
}


subset_by_from.default <- function (from, data) {
  if (is.null(names(from))) names(from) <- c("xmin", "xmax", "ymin", "ymax")
  if (inherits(data$geometry, "sfc")) {
    rlang::check_installed("sf")
    bb <- sf::st_bbox(unlist(from))
    bb <- sf::st_as_sfc(bb) # `crs =` doesn't work
    bb <- sf::st_set_crs(bb, sf::st_crs(data$geometry))
    in_bounds <- sf::st_within(data$geometry, bb)
    in_bounds <- lengths(in_bounds) > 0
    data[in_bounds,]
  } else {
    cond <- data$x >= from["xmin"] & data$x <= from["xmax"] &
            data$y >= from["ymin"] & data$y <= from["ymax"]
    data[cond,]
  }
}
