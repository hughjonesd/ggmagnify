
skip_on_ci()
library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species, shape = Species))
ggp2 <- ggp + geom_point()

test_that("basics", {
  expect_silent(
    ggp2 + geom_magnify(from = c(2.5, 5.5, 3.0, 6.0),
                        to = c(3.5, 4.5, 4.5, 5.5))
  )

  expect_snapshot_file(
    ggsave("test-basics-basics.png", width = 5, height = 5)
  )
})

test_that("reversed", {

  ggp_rev <- ggp2 + coord_cartesian(xlim = c(6,2))

  expect_no_error(
    ggp_rev + geom_magnify(from = c(2.5, 5.5, 3.0, 6.0),
                           to = c(3.5, 4.5, 4.5, 5.5))
  )

  expect_snapshot_file(
    ggsave("test-basics-reversed.png", width = 5, height = 5)
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
