
library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species, shape = Species))
ggp2 <- ggp + geom_point()


test_that("facet_wrap", {
  expect_silent(
    ggp2 + facet_wrap(vars(Species)) +
      geom_magnify_tile(x = 3, width = 2, y = 5, height = 1.5, to_x = 3, to_y = 7,
                   to_width = 2, to_height = 1.5)
  )

  skip_on_ci()
  expect_snapshot_file(
    ggsave("test-geom-facet-wrap.png", width = 5, height = 5)
  )
})


test_that("facet_grid", {
  ggp3 <- ggplot(mtcars, aes(mpg, qsec, color = cyl)) +
          geom_point() +
          facet_grid(vars(am), vars(vs))

  expect_silent(
    ggp3 +
      geom_magnify_tile(x = 15, width = 4, y = 21, height = 2, to_x = 30, to_y = 18,
                   to_width = 8, to_height = 4)
  )

  skip_on_ci()
  expect_snapshot_file(
    ggsave("test-geom-facet-grid.png", width = 5, height = 5)
  )

  expect_silent(
    ggp3 + facet_grid(vars(am), vars(vs), scales = "free") +
      geom_magnify_tile(x = 15, width = 4, y = 18, height = 2, to_x = 30, to_y = 21,
                   to_width = 8, to_height = 4)
  )

  skip_on_ci()
  expect_snapshot_file(
    ggsave("test-geom-facet-free.png", width = 5, height = 5)
  )
})

