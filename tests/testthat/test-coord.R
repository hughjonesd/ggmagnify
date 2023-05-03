
library(ggplot2)

library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species, shape = Species))
ggp2 <- ggp + geom_point()

from <- c(3, 6, 3.5, 7)
to <- c(2.4, 4.3, 3.2, 5.7)


test_that("coord_fixed", {
  expect_no_error(
    ggp2 + coord_fixed() + geom_magnify(from = from, to = to)
  )
})


test_that("coord_trans", {
  # 60% of the time, it works all the time!
  expect_no_error(
    ggp2 + coord_trans(x = "log10", y = "log10") +
      geom_magnify(from = from - c(0.5, 0, 0.5, 0), to = to + c(1, 0, 1, 0))
  )
})


test_that("coord_polar", {
  skip("Not yet!")
})


test_that("coord_sf", {
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ggp <- ggplot(nc) +
    geom_sf(aes(fill = AREA)) +
    coord_sf(default_crs = sf::st_crs(4326))

  xlim <- c(-79, -77)
  ylim <- c(34.5, 35)
  expect_no_error(
    ggp + geom_magnify(from = c(-79, 34.5, -77, 35),
                       to = c(-84, 34, -80, 35))
  )
})
