
skip_on_ci()
library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species, shape = Species))
ggp2 <- ggp + geom_point()

from <- c(2.5, 3.0, 5.5, 6.0)
to <- c(3.5, 4.5, 4.5, 5.5)

expect_plot <- function (expr) {
  expr <- enquo(expr)
  expect_silent(eval(expr))
  expect_silent(print(last_plot()))
}


test_that("basics", {
  expect_silent(
    ggp2 + geom_magnify(from = from, to = to)
  )

  expect_snapshot_file(
    ggsave("test-basics-basics.png", width = 5, height = 5)
  )
})


test_that("aesthetics", {
  expect_plot(
    ggp2 + geom_magnify(aes(from = list(from)), to = to)
  )
  expect_plot(
    ggp2 + geom_magnify(aes(to = list(to)), from = from)
  )
  expect_plot(
    ggp2 + geom_magnify(aes(to = list(to), from = list(from)))
  )
})


test_that("versions of from", {
  expect_plot(
    ggp2 + geom_magnify(aes(from = Species == "versicolor" & Sepal.Length < 6),
                        to = to)
  )
  expect_plot(
    ggp2 + geom_magnify(from = iris$Species == "versicolor" & iris$Sepal.Length < 6,
                        to = to)
  )

  d <- data.frame(x = seq(3.0, 3.5, length = 5), y = seq(6.5, 7.0, length = 5))
  expect_plot(
    ggp2 + geom_magnify(aes(from = list(d)), to = to)
  )
  expect_plot(
    ggp2 + geom_magnify(from = d, to = to)
  )
  # we did list above already
  #

  from_grob <- grid::circleGrob(x = 2.75, y = 6.25, r = 0.5,
                              default.units = "native")
  expect_plot(
    ggp2 + geom_magnify(aes(from = list(from_grob)), to = to)
  )
})


test_that("shape = \"ellipse\"", {
  expect_plot(
    ggp2 + geom_magnify(from = from, to = to, shape = "ellipse")
  )
  expect_plot(
    ggp2 + geom_magnify(aes(from = Species == "versicolor" & Sepal.Length < 6),
                        to = to, shape = "ellipse")
  )
  expect_plot(
    ggp2 + geom_magnify(aes(from = Species == "versicolor" & Sepal.Length < 6),
                        to = to, shape = "hull")
  )
})


test_that("shape = \"hull\"", {
  expect_plot(
    ggp2 + geom_magnify(aes(from = Species == "versicolor" & Sepal.Length < 6),
                        to = to, shape = "hull")
  )

  svsl6 <- iris$Species == "versicolor" & iris$Sepal.Length < 6
  d <- data.frame(x = iris$Sepal.Width[svsl6], y = iris$Sepal.Length[svsl6])
  expect_plot(
    ggp2 + geom_magnify(from = d, to = to, shape = "hull")
  )

  expect_silent(
    ggp2 + geom_magnify(from = from, to = to, shape = "hull")
  )
  expect_warning(
    print(last_plot())
  )
})


test_that("clipping", {
  ggp2 + geom_magnify_tile(x = 3, width = 2, y = 5, height = 1.5, to_x = 4, to_y = 7,
                       to_width = 2, to_height = 1.5)

  expect_snapshot_file(
    ggsave("test-basics-clipping-rect.png", width = 5, height = 5)
  )

  ggp2 +
    geom_magnify_tile(x = 3, width = 2, y = 5, height = 1.5, to_x = 4, to_y = 7,
                 to_width = 2, to_height = 1.5, shape = "ellipse")

  expect_snapshot_file(
    ggsave("test-basics-clipping-ellipse.png", width = 5, height = 5)
  )
})


file.remove(list.files(pattern = "test-basics.*png"))
