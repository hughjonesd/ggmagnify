

df_to_grob <- function (df) {
  # the [[]] matters so we get vectors
  grid::polygonGrob(x = df[[1]], y = df[[2]], default.units = "native")
}


# The below are derived from ggplot2 code

incremental_id <- local({
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
