


calculate_proj_segments <- function(proj, shape,
                                    xmin, xmax, ymin, ymax,
                                    to_xmin, to_xmax, to_ymin, to_ymax) {
  shape <- structure(list(), class = shape)
  UseMethod("calculate_proj_segments", shape)

}


calculate_proj_segments.rect <- function(proj, shape,
                                    xmin, xmax, ymin, ymax,
                                    to_xmin, to_xmax, to_ymin, to_ymax) {
  x <- (xmin + xmax)/2
  y <- (ymin + ymax)/2
  to_x <- (to_xmin + to_xmax)/2
  to_y <- (to_ymin + to_ymax)/2

  if (proj %in% c("auto", "corresponding", "facing")) {
    # which of the four lines connecting the four corners can we draw?
    can_top_left  <- sign(xmin - to_xmin) == sign(ymax - to_ymax)
    can_bot_right <- sign(xmax - to_xmax) == sign(ymin - to_ymin)
    can_bot_left  <- sign(xmin - to_xmin) != sign(ymin - to_ymin)
    can_top_right <- sign(xmax - to_xmax) != sign(ymax - to_ymax)
    can_proj <- c(can_bot_left, can_top_left, can_bot_right, can_top_right)

    proj_x    <- c(xmin, xmin, xmax, xmax)
    proj_y    <- c(ymin, ymax, ymin, ymax)
    proj_xend <- c(to_xmin, to_xmin, to_xmax, to_xmax)
    proj_yend <- c(to_ymin, to_ymax, to_ymin, to_ymax)

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
        if (to_ymin > ymax) {
          # "always project the top of the target to the bottom of the inset"
          proj_y <- rep(ymax, 4)
          proj_yend <- rep(to_ymin, 4)
        } else if (to_ymax < ymin) {
          proj_y <- rep(ymin, 4)
          proj_yend <- rep(to_ymax, 4)
        }
      }
      if (adjacent_vert) {
        if (to_xmin > xmax) {
          proj_x <- rep(xmax, 4)
          proj_xend <- rep(to_xmin, 4)
        } else if (to_xmax < xmin) {
          proj_x <- rep(xmin, 4)
          proj_xend <- rep(to_xmax, 4)
        }
      }
    }
  } else if (proj == "single") {
    # t r b l midpoints of the target, opposite side of the inset:
    proj_x    <- c(x, xmax, x, xmin)
    proj_y    <- c(ymax, y, ymin, y)
    proj_xend <- c(to_x, to_xmin, to_x, to_xmax)
    proj_yend <- c(to_ymin, to_y, to_ymax, to_y)

    gaps <- c(to_ymin - ymax, # top (of target, bottom of inset)
              to_xmin - xmax, # right
              ymin - to_ymax, # bottom
              xmin - to_xmax) # left

    # We try to pick the "biggest" gap to make the line angle least acute.
    # We don't know the dimensions of the plot; we guess by using the
    # inset (theorizing it will have "sensible" dimensions). This is hacky...
    gaps <- gaps/c(to_ymax - to_ymin, to_xmax - to_xmin, to_ymax - to_ymin,
                   to_xmax - to_xmin)
    can_proj <- which.max(gaps)
  }

  proj_x    <- proj_x[can_proj]
  proj_y    <- proj_y[can_proj]
  proj_xend <- proj_xend[can_proj]
  proj_yend <- proj_yend[can_proj]

  return(data.frame(x = proj_x, y = proj_y, xend = proj_xend, yend = proj_yend))
}



calculate_proj_segments.ellipse <- function(proj, shape,
                                 xmin, xmax, ymin, ymax,
                                 to_xmin, to_xmax, to_ymin, to_ymax) {
  e1 <- ellipse_points(data.frame(xmin, xmax, ymin, ymax))
  e2 <- ellipse_points(data.frame(xmin = to_xmin, xmax = to_xmax,
                                  ymin = to_ymin, ymax = to_ymax))

  dydx <- function (ell) {
    dy <- c(NA, diff(ell$y))
    dy[1] <- ell$y[1] - ell$y[length(ell$y)]
    dx <- c(NA, diff(ell$x))
    dx[1] <- ell$x[1] - ell$x[length(ell$x)]
    atan(dy/dx)
  }

  e1$dydx <- dydx(e1)
  e2$dydx <- dydx(e2)

  # we look for a "top point" and a "bottom point" on either side of the line
  # between the two points
  midpoint1 <- c((xmin + xmax)/2, (ymin + ymax)/2)
  midpoint2 <- c((to_xmin + to_xmax)/2, (to_ymin + to_ymax)/2)
  midpoint_vec <- midpoint2 - midpoint1

  # avoid division by 0 in the below
  j <- 1
  if (midpoint_vec[1] == 0) j <- 2
  k <- 3 - j
  above_midpoint_vec <- function(pt) {
    scaled_vec <- midpoint1 + midpoint_vec * (pt[j] - midpoint1[j])/midpoint_vec[j]
    pt[k] > scaled_vec[k]
  }

  e1$above <- apply(e1[c("x", "y")], 1, above_midpoint_vec)
  e2$above <- apply(e2[c("x", "y")], 1, above_midpoint_vec)

  find_tangent <- function (e1, e2) {
    e2 <- e2[c("x", "y", "dydx")]
    closest <- sapply(e1$dydx, function (x) which.min(abs(x - e2$dydx)))

    names(e2) <- c("xend", "yend", "dydx2")
    res <- cbind(e1[c("x", "y", "dydx")], e2[closest, ])

    res$dydx_slope <- with(res, atan((yend - y)/(xend - x)))
    slope_diffs <- with(res, abs(dydx_slope - dydx) + abs(dydx_slope - dydx2))
    res[which.min(slope_diffs), ]
  }

  tangent_above <- find_tangent(e1[e1$above,], e2[e2$above,])
  tangent_below <- find_tangent(e1[!e1$above,], e2[!e2$above,])

  rbind(tangent_above, tangent_below)
}
