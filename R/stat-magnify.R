
StatMagnify <- ggproto("StatMagnify", Stat,
  required_aes = c("from", "to"),

  # note: these parameters do magic by computing Stat$parameters()
  compute_panel = function (data, scales, from, to, shape, expand,
                            aspect) {
    # we use only the first line of data$from *unless* it's a logical vector when
    # we use all of it
    # a bit hacky?
    # note that if `from` is in the data then it may be a list-column.
    # The double indexing below gets rid of the `list()`, though.
    from <- if ("from" %in% names(data)) {
              if (is.logical(data$from)) data$from else data$from[[1]]
            }  else {
              from
            }
    from_bounds <- find_bounds(from, shape, data)

    data$xmin <- from_bounds[["xmin"]]
    data$ymin <- from_bounds[["ymin"]]
    data$xmax <- from_bounds[["xmax"]]
    data$ymax <- from_bounds[["ymax"]]

    data[c("inset_xmin", "inset_ymin", "inset_xmax", "inset_ymax")] <-
      data[c("xmin", "ymin", "xmax", "ymax")]

    to <- data$to[[1]] %||% to
    if (is.null(names(to))) {
      names(to) <- c("xmin", "xmax", "ymin", "ymax")
    }
    data$to_xmin <- to[["xmin"]]
    data$to_ymin <- to[["ymin"]]
    data$to_xmax <- to[["xmax"]]
    data$to_ymax <- to[["ymax"]]

    transform_x <- ! is.null(scales$x) && ! scales$x$is_discrete()
    transform_y <- ! is.null(scales$y) && ! scales$y$is_discrete()

    if (transform_x) {
      # don't use transform_df, it won't deal with to_xxx
      data$xmin <- scales$x$transform(data$xmin)
      data$xmax <- scales$x$transform(data$xmax)
      data$to_xmin <- scales$x$transform(data$to_xmin)
      data$to_xmax <- scales$x$transform(data$to_xmax)
    }
    if (transform_y) {
      data$ymin <- scales$y$transform(data$ymin)
      data$ymax <- scales$y$transform(data$ymax)
      data$to_ymin <- scales$y$transform(data$to_ymin)
      data$to_ymax <- scales$y$transform(data$to_ymax)
    }

    # this should be done after transformation because we want it fixed on
    # screen, rather than in data terms
    if (identical(aspect, "fixed")) {
      magnification <- as.numeric(data$to_xmax - data$to_xmin)/
        as.numeric(data$xmax - data$xmin)
      data$to_ymax <- data$to_ymin + (data$ymax - data$ymin) * magnification
    }

    for (prefix in c("x", "y", "to_x", "to_y", "inset_x", "inset_y")) {
      blah_min <- paste0(prefix, "min")
      blah_max <- paste0(prefix, "max")
      row1 <- c(data[[1, blah_min]], data[[1, blah_max]]) # double brackets matter
      expanded <- expand_by(row1, expand)
      # ugly way to do all rows at once
      data[blah_min] <- expanded[1]
      data[blah_max] <- expanded[2]
    }

    data
  }
)


#' Calculate `xmin, ymin, xmax, ymax` from the `from` aes
#'
#' @param from Original aes, can be logical, data frame, list, vector or grob
#' @param shape Parameter from `geom_magnify`
#' @param data Data passed in to `compute_panel`
#'
#' @return A list with components xmin, xmax, ymin, ymax
#' @noRd
find_bounds <- function (from, shape, data) {
  UseMethod("find_bounds")
}


find_bounds.data.frame <- function (from, shape, data) {
  list(xmin = min(from$x), ymin = min(from$y), xmax = max(from$x),
       ymax = max(from$y))
}


find_bounds.logical <- function (from, shape, data) {
  if (is.null(data$x) && inherits(data$geometry, "sfc")) {
    rlang::check_installed("sf")
    bb <- sf::st_bbox(data$geometry[from])
    bb <- as.list(bb)
    bb[c("xmin", "xmax", "ymin", "ymax")]
  } else {
    x <- data$x[from]
    y <- data$y[from]
    list(xmin = min(x), ymin = min(y), xmax = max(x), ymax = max(y))
  }
}


find_bounds.numeric <- function (from, shape, data) {
  if (is.null(names(from))) {
    names(from) <- c("xmin", "xmax", "ymin", "ymax")
  }

  as.list(from)
}


find_bounds.list <- find_bounds.numeric


find_bounds.grob <- function (from, shape, data) {
  from <- allcoords(from, bind = TRUE)
  list(xmin = min(from[, "x"]), ymin = min(from[, "y"]),
       xmax = max(from[, "x"]), ymax = max(from[, "y"]))
}
