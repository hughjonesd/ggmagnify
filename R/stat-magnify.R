
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
    params$aspect <- match.arg(params$aspect, c("free", "fixed"))


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
                            to_ymax = NULL, aspect = NULL) {
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
    if (identical(aspect, "fixed")) {
      magnification <- (data$to_xmax - data$to_xmin)/(data$xmax - data$xmin)
      data$to_ymax <- data$to_ymin + (data$ymax - data$ymin) * magnification
    }

    data
  }
)
