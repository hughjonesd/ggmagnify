
skip_on_ci()
library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species, shape = Species))
ggp2 <- ggp + geom_point()

from <- c(2.5, 5.5, 3.0, 6.0)
to <- c(3.5, 4.5, 4.5, 5.5)

test_that("basics", {
  expect_silent(
    ggp2 + geom_magnify(from = from,
                        to = to)
  )

  expect_snapshot_file(
    ggsave("test-basics-basics.png", width = 5, height = 5)
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


test_that("setting aesthetics", {
  expect_silent({
    ggm_set <- ggp2 + geom_magnify(xmin = from[1], ymin = from[2],
                                   xmax = from[3], ymax = from[4],
                                   to_xmin = to[1], to_ymin = to[2],
                                   to_xmax = to[3], to_ymax = to[4])
    print(ggm_set)
  })

  skip_on_ci()
  expect_snapshot_file(
    ggsave("test-basics-setting-aesthetics.png", ggm_set, width = 5, height = 5)
  )
})

test_that("position from data", {
  d <- data.frame(xmin = from[1], ymin = from[2], xmax = from[3],
                  ymax = from[4],
                  to_xmin = to[1], to_ymin = to[2], to_xmax = to[3],
                  to_ymax = to[4])

  expect_silent({
    ggm_pos_data <- ggp2 + geom_magnify(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
          to_xmin = to_xmin, to_xmax = to_xmax, to_ymin = to_ymin, to_ymax = to_ymax),
      data = d)
    print(ggm_pos_data)
  })

  # mix of data and set aesthetics
  expect_silent({
    ggm_mix_data <- ggp2 + geom_magnify(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
          to_xmin = to_xmin, to_xmax = to_xmax, to_ymin = to_ymin,
          to_ymax = to_ymax),
      data = d,
      ymax = from[4] - .2, # set parameters
      to_ymax = to[4] + 1)

    print(ggm_mix_data)
  })

  expect_silent({
    ggmt_pos_data <- ggp2 + geom_magnify_tile(
      aes(x = x, y = y, width = width, height = height,
          to_x = to_x, to_y = to_y, to_width = to_width, to_height = to_height
          ),
      data = data.frame(x = 2.75, y = 5.75,  width = .5, height = .5,
                        to_x = 4, to_y = 5, to_width = 1, to_height = 1)
    )
    print(ggmt_pos_data)
  })

  skip_on_ci()
  expect_snapshot_file(
    ggsave("test-basics-position-from-data.png", ggm_pos_data,
           width = 5, height = 5)
  )
  expect_snapshot_file(
    ggsave("test-basics-position-mix-data.png", ggm_mix_data,
           width = 5, height = 5)
  )
  expect_snapshot_file(
    ggsave("test-basics-tile-position-from-data.png", ggmt_pos_data,
           width = 5, height = 5)
  )
})


file.remove(list.files(pattern = "test-basics.*png"))
