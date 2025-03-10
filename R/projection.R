

calculate_proj_df <- function (proj, proj.combine, grob1, grob2) {
  c1 <- allcoords(grob1, bind = proj.combine)
  c2 <- allcoords(grob2, bind = proj.combine)
  if (proj.combine) {
    c1 <- list(c1)
    c2 <- list(c2)
  }

  calculate_proj <- if (proj == "single") calculate_proj_midpoint else
                                          calculate_proj_chull
  proj_line_list <- lapply(seq_along(c1),
                           function (i) calculate_proj(c1[[i]], c2[[i]])
                    )
  do.call(rbind, proj_line_list)
}


calculate_proj_chull <- function (c1, c2) {
  both <- rbind(c1, c2)
  on_chull <- grDevices::chull(both)
  both <- as.data.frame(both)
  names(both) <- c("x", "y")
  both$grob <- c(rep(1, nrow(c1)), rep(2, nrow(c2)))

  both <- both[on_chull,] # now in "clockwise order"
  # points on the chull where we move from one grob to another
  switching <- diff(both$grob) != 0
  # make it "circular"
  switching <- c(switching, both$grob[nrow(both)] != both$grob[1])
  switching <- which(switching)
  switched <- switching + 1
  switched <- (switched - 1) %% nrow(both) + 1 # starts again at 1 not 0

  # we draw lines from switching points to switched points
  from <- both[switching, c("x", "y")]
  to <- both[switched, c("x", "y")] # if row 1 is
  names(to) <- c("xend", "yend")
  proj_df <- cbind(from, to)

  # lastly we switch the start-end order of the second line
  # to make both lines go in the same direction
  # this makes our life easier when filling
  proj_df[2,] <- proj_df[2, c("xend", "yend", "x", "y")]
  proj_df
}


calculate_proj_midpoint <- function(c1, c2) {
  stopifnot(nrow(c1) == nrow(c2))

  # this is how grid defines "centre" for xDetails
  c1r <- apply(c1, 2, range)
  c2r <- apply(c2, 2, range)
  mp1 <- apply(c1r, 2, mean)
  mp2 <- apply(c2r, 2, mean)
  mp_vec <- mp2 - mp1
  # theta <- atan(mp_vec[2]/mp_vec[1]) * 360 / (2 * pi)
  # if (mp_vec[2] == 0 && mp_vec[1] < 0) theta <- 180

  c1_in_bbox <- in_bbox(c1, mp1, mp2)
  c2_in_bbox <- in_bbox(c2, mp1, mp2)

  # find closest point to mp_vec in bounding box
  # rather than bounding box, we want to  look at all points between
  # lines orthogonal to mp2 - mp1
  orthog_mp_vec <- c(mp_vec[2], - mp_vec[1])
  between_midpoints <- function (cc) {
    rv_mp1 <- rejection_vec(cc, orthog_mp_vec, mp1)
    d1 <- rejection_vec(matrix(mp2, 1, 2), orthog_mp_vec, mp1)[,1]
    in_mp1 <- rv_mp1[, 1]  *  d1 >= 0
    rv_mp2 <- rejection_vec(cc, orthog_mp_vec, mp2)
    d2 <- rejection_vec(matrix(mp1, 1, 2), orthog_mp_vec, mp2)[,1]
    in_mp2 <- rv_mp2[, 1]  * d2 >= 0
    in_mp1 & in_mp2
  }

  c1 <- c1[between_midpoints(c1),]
  c2 <- c2[between_midpoints(c2),]
  c1_dist_mp_vec <- dist_vec(c1, mp2, mp1)
  c2_dist_mp_vec <- dist_vec(c2, mp2, mp1)
  closest1 <- c1[which.min(c1_dist_mp_vec), ]
  closest2 <- c2[which.min(c2_dist_mp_vec), ]
  res <- data.frame(x = closest1[1], y = closest1[2], xend = closest2[1],
                    yend = closest2[2])

  return(res)
}


#' Turn a grob into coordinates
#'
#' @param grob A grid::grob
#' @param bind TRUE to rbind the matrices together.
#'
#' @return (List of) matrices of x and y on the scale of data
#' (default.units = "native"). One matrix per polygon.
#' @noRd
allcoords <- function (grob, bind) {
  gc <- grid::grobCoords(grob, closed = TRUE)
  list_cc <- lapply(gc, function(l) as.data.frame(l[c("x", "y")]))
  convert_units <- function (cc) {
    cc1 <- unit(cc[, 1], "inches") # can't store units in a matrix
    cc2 <- unit(cc[, 2], "inches")
    cc1 <- convertX(cc1, "native", valueOnly = TRUE)
    cc2 <- convertY(cc2, "native", valueOnly = TRUE)
    cbind(x = cc1, y = cc2)
  }

  list_cc <- lapply(list_cc, convert_units)
  if (bind) do.call(rbind, list_cc) else list_cc
}

#' Vectors of "rejections"   at 90 deg to line through `vec` & `origin`
#'
#' @param pts n by 2 matrix
#' @param vec length 2
#' @param origin length 2
#'
#' @return n x 2 matrix
#' @noRd
rejection_vec <- function (pts, vec, origin) {
  if (nrow(pts) > 0) pts <- pts - matrix(origin, nrow(pts), 2, byrow = TRUE)
  vec <- vec - origin
  a.b <- pts %*% vec # n x 1
  b.b <- t(vec) %*% vec # 1x1
  on_vec <- (a.b/c(b.b)) %*% vec # n x 2 matrix of projections
  on_vec - pts # n x 2 matrix of rejections
}


dist_vec <- function(pts, vec, origin) {
  rej_vec <- rejection_vec(pts, vec, origin)
  rowSums(rej_vec^2) # distances
}


#' Are pts in bounding box with corners p1 , p2
#'
#' @param pts n x 2 matrix
#' @param p1,p2 2 length vector
#'
#' @return n length logical vector
#' @noRd
in_bbox <- function(pts, p1, p2) {
  vec <- p2 - p1

  (pts[, 1] - p1[1]) * vec[1] >= 0 &
  (pts[, 2] - p1[2]) * vec[2] >= 0 &
  (pts[, 1] - p2[1]) * vec[1] <= 0 &
  (pts[, 2] - p2[1]) * vec[2] <= 0
  # sign(pts[, 1] - p1[1]) == sign(vec[1]) &
  # sign(pts[, 2] - p1[2]) == sign(vec[2]) &
  # sign(pts[, 1] - p2[1]) != sign(vec[1]) &
  # sign(pts[, 2] - p2[2]) != sign(vec[2])
}


calculate_proj_df_rect <- function(proj, data, corners, coord, panel_params) {
  xmin <- data$xmin
  xmax <- data$xmax
  ymin <- data$ymin
  ymax <- data$ymax
  to_xmin <- data$to_xmin
  to_xmax <- data$to_xmax
  to_ymin <- data$to_ymin
  to_ymax <- data$to_ymax

  if (corners > 0 && ! identical(proj, "single")) {
    # a 1 - 1/sqrt(2) adjustment, c. 0.29, would get to the midpoint of the
    # rounded corner (at a 45 degree angle), calculated via Pythagoras.
    # This isn't perfect because lines don't always come in at 45 deg.
    # An adj of 0.2 is a bit "looser" and should give fewer cases
    # where projection lines end up inside the target area.
    # The use of min() below reflects that corners is in "snpc" units:
    # see ?grid::unit.
    size <- min(xmax - xmin, ymax - ymin)
    to_size <- min(to_xmax - to_xmin, to_ymax - to_ymin)
    adj <- 0.2 # 1 - 1/sqrt(2)
    corn_adj <- corners * size * adj
    to_corn_adj <- corners * to_size * adj

    xmin <- xmin + corn_adj
    xmax <- xmax - corn_adj
    ymin <- ymin + corn_adj
    ymax <- ymax - corn_adj

    to_xmin <- to_xmin + to_corn_adj
    to_xmax <- to_xmax - to_corn_adj
    to_ymin <- to_ymin + to_corn_adj
    to_ymax <- to_ymax - to_corn_adj
  }

  # using mean allows Dates and maybe other things
  x <- mean(c(xmin, xmax))
  y <- mean(c(ymin, ymax))
  to_x <- mean(c(to_xmin, to_xmax))
  to_y <- mean(c(to_ymin, to_ymax))

  if (proj %in% c("corresponding", "facing")) {
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

  if (! any(can_proj)) {
    cli::cli_warn(c("Can't draw projection lines.",
                  "*" = "Does inset cover target area or vice versa?"))
  }

  proj_x    <- proj_x[can_proj]
  proj_y    <- proj_y[can_proj]
  proj_xend <- proj_xend[can_proj]
  proj_yend <- proj_yend[can_proj]

  df <- data.frame(x = proj_x, y = proj_y, xend = proj_xend, yend = proj_yend)
  coord$transform(df, panel_params)
}


#' Creates a grob to fill between the projection lines
#'
#' @param proj_df Data frame returned from calculate_proj_df_*.
#'   This should have n pairs of matched projection lines,
#'   both going in the same direction (i.e. from target to inset or vice versa)
#' @param target_grob From [draw_panel()].
#' @param fill The fill colour
#'
#' @return The grob, excluding the target grob.
#' @noRd
make_proj_fill_grob <- function (proj_df, target_grob, fill) {
  line_pair_idx <- seq(1L, nrow(proj_df), by = 2L)

  proj_fill_grobs <- lapply(line_pair_idx,
                            function (idx) {
    x <- c(proj_df$x[idx],
           proj_df$xend[idx],
           proj_df$xend[idx + 1],
           proj_df$x[idx + 1])
    y <- c(proj_df$y[idx],
           proj_df$yend[idx],
           proj_df$yend[idx + 1],
           proj_df$y[idx + 1])

    unclipped_grob <- polygonGrob(x = x, y = y, default.units = "native")
    gridGeometry::polyclipGrob(unclipped_grob, target_grob, op = "minus",
                               gp = gpar(col  = "transparent", fill = fill))
  })

  proj_fill_grobs <- do.call(gList, proj_fill_grobs)
  gTree(children = proj_fill_grobs)
}
