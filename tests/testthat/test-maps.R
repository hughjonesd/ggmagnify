
library(ggplot2)

test_that("ggmagnify works with maps", {
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ggp <- ggplot(nc) +
    geom_sf(aes(fill = AREA)) +
    coord_sf(default_crs = sf::st_crs(4326))

  xlim <- c(-79, -77)
  ylim <- c(34.5, 35)
  expect_error(
    ggmagnify(ggp, xlim = xlim, ylim = ylim,
              inset_xlim = c(-84, -80), inset_ylim = c(34, 35),
              inset_coord = coord_sf(default_crs = sf::st_crs(4326),
                                      xlim = xlim, ylim = ylim),
              shadow = TRUE)
    , regexp = NA
  )
})
