
library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species, shape = Species))
ggp2 <- ggp + geom_point()

test_that("basics", {
  expect_silent(
    ggp2 +
      geom_magnify(x = 3, width = 2, y = 5, height = 1.5, to_x = 3, to_y = 7,
                   magnify = 1)
  )

  expect_snapshot_file(
    ggsave("test-geom-basics.png", width = 5, height = 5)
  )
})

test_that("reversed", {

  ggp_rev <- ggp2 + coord_cartesian(xlim = c(6,2))

  expect_no_error(
    ggp_rev + geom_magnify(x = 3, width = 1, y = 5, height = 1, to_x = 5, to_y = 7,
                   magnify = 1.5)
  )

  expect_snapshot_file(
    ggsave("test-geom-reversed.png", width = 5, height = 5)
  )
})


test_that("facets", {
  expect_silent(
    ggp2 + facet_wrap(vars(Species)) +
      geom_magnify(x = 3, width = 2, y = 5, height = 1.5, to_x = 3, to_y = 7,
                   magnify = 1)
  )

  expect_snapshot_file(
    ggsave("test-geom-facet-wrap.png", width = 5, height = 5)
  )

  ggp3 <- ggplot(mtcars, aes(mpg, qsec, color = cyl)) +
          geom_point() +
          facet_grid(vars(am), vars(vs))

  expect_silent(
    ggp3 +
      geom_magnify(x = 15, width = 4, y = 21, height = 2, to_x = 30, to_y = 18,
                   magnify = 2)
  )

  expect_snapshot_file(
    ggsave("test-geom-facet-grid.png", width = 5, height = 5)
  )

  expect_silent(
    ggp3 + facet_grid(vars(am), vars(vs), scales = "free") +
      geom_magnify(x = 15, width = 4, y = 18, height = 2, to_x = 30, to_y = 21,
                   magnify = 2)
  )

  expect_snapshot_file(
    ggsave("test-geom-facet-free.png", width = 5, height = 5)
  )
})

file.remove(list.files(pattern = "test-geom.*png"))
