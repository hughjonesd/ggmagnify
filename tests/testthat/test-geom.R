
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
    ggsave("geom-basics.png", width = 5, height = 5)
  )
})
