


calculate_proj_segments <- function(proj, shape,
                                    xmin, xmax, ymin, ymax,
                                    to_xmin, to_xmax, to_ymin, to_ymax) {

  if (shape == "rect") {
    calculate_proj_segments_rect(proj,
                                 xmin, xmax, ymin, ymax,
                                 to_xmin, to_xmax, to_ymin, to_ymax)
  } else {
    x <- (xmin + xmax)/2
    y <- (ymin + ymax)/2
    to_x <- (to_xmin + to_xmax)/2
    to_y <- (to_ymin + to_ymax)/2
    r <- min(xmax - xmin, ymax - ymin)
    to_r <- min(to_xmax - to_xmin, to_ymax - to_ymin)
    calculate_proj_segments_circle(proj, x, y, r, to_x, to_y, to_r)
  }
}


calculate_proj_segments_rect <- function(proj,
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


calculate_proj_segments_circle <- function (proj, x, y, r, to_x, to_y, to_r) {
  dx <- to_x - x
  dy <- to_y - y

  # if proj not "single", take 90deg to this angle. shift out by +-r from
  # (x,y) and +- to_r from (to_x, to_y)
  # if proj "single" shift forward by r from (x,y) and backward by to_r from (to_x, to_y)

   # go up by r at angle of angle. What is r_x and r_y?
    # r_x^2 + r_y^2 = r^2
    # r_y/r_x = dy/dx
    # r_y = dy/dx * r_x
    # r_x^2 + r_x^2 * (dy/dx)^2 = r^2
    # r_x^2 * (1 + (dy/dx)^2) = r^2
    # r_x^2 = r^2/((1 + (dy/dx)^2)
    # r_x = r/sqrt(1+(dy/dx)^2)
  r_x <- r/sqrt(1 + (dy/dx)^2)
  r_y <- dy/dx * r_x
  to_r_x <- to_r/sqrt(1 + (dy/dx)^2)
  to_r_y <- dy/dx * to_r_x
  if (proj == "single") {
    x <- x + r_x
    y <- y + r_y
    xend <- to_x - to_r_x
    yend <- to_y - to_r_y
  } else {
    # x<- x + r_y, y <- y - r_x
    x <- x + c(-1,1) * r_y
    y <- y + c(1, -1) * r_x
    xend <- to_x + c(-1, 1) * to_r_y
    yend <- to_y + c(1, -1) * to_r_x
  }

  data.frame(x = x, y = y, xend = xend, yend = yend)
}
