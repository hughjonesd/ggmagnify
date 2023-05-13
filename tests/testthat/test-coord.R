
library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species, shape = Species))
ggp2 <- ggp + geom_point()

from <- c(3, 3.5, 6, 7)
to <- c(2.4, 3.2, 4.3, 5.7)



test_that("reversed coords", {
  ggp_rev_x <- ggp2 + coord_cartesian(xlim = c(6, 2)) +
    geom_magnify(from = from, to = to)

  expect_no_error(
    print(ggp_rev_x)
  )

  ggp_rev_y <- ggp2 +
    coord_cartesian(ylim = c(8, 4)) +
    geom_magnify(from = from, to = to)

  expect_no_error(
    print(ggp_rev_y)
  )

  skip_on_ci()
  expect_snapshot_file(
    ggsave("test-scales-reversed-coords-x.png", ggp_rev_x, width = 5, height = 5)
  )
  expect_snapshot_file(
    ggsave("test-scales-reversed-coords-y.png", ggp_rev_y, width = 5, height = 5)
  )
})


test_that("coord_fixed", {
  expect_no_error(
    ggp2 + coord_fixed() + geom_magnify(from = from, to = to)
  )
})


test_that("coord_trans", {
  # 60% of the time, it works all the time!
  expect_no_error(
    ggp2 + coord_trans(x = "log10", y = "log10") +
      geom_magnify(from = from - c(0.5, 0.5, 0, 0), to = to + c(1, 1, 0, 0))
  )
})


test_that("coord_polar", {
  skip("Not implemented")
})


test_that("coord_sf", {
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ggp <- ggplot(nc) +
    geom_sf(aes(fill = AREA)) +
    coord_sf(default_crs = sf::st_crs(4326))

  expect_no_error(
    ggp + geom_magnify(from = c(-79, -77, 34.5, 35),
                       to = c(-84, -80, 34, 35))
  )
})
