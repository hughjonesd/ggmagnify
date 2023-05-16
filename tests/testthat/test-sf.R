

library(ggplot2)

skip_if_not_installed("sf")
skip_if_not_installed("maps")


usa <- sf::st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
world <- sf::st_as_sf(maps::map("world", fill = TRUE, plot = FALSE))

ggpm <- ggplot(usa) +
  geom_sf() +
  coord_sf(default_crs = sf::st_crs(4326), ylim = c(10, 50)) +
  theme(legend.position = "none")

ggpw <- ggplot(world) +
  geom_sf() +
  coord_sf(default_crs = sf::st_crs(4326), xlim = c(-40, 40),
           ylim = c(0, 60))

expect_snap_plot <- function (name, expr) {
  expr <- enquo(expr)
  expect_silent(ggp_res <- rlang::eval_tidy(expr))

  expect_silent(print(ggp_res))

  name <- gsub(" ", "-", name, fixed = TRUE)
  name <- paste0("test-sf-", name, ".png")
  expect_snapshot_file(ggsave(name, ggp_res, width = 4, height = 4))
}


test_that("rect", {
  expect_snap_plot("rect with from list",
    ggpm + geom_magnify(from = c(-110, -100, 35, 45),
                        to = c(-125, -105, 10, 30),
                        aspect = "fixed",
                        expand = 0)
  )

  expect_snap_plot("rect with from aes",
    ggpm + geom_magnify(aes(from = ID == "texas"),
                        to = c(-125, -105, 10, 30),
                        aspect = "fixed",
                        expand = 0)
  )
})


test_that("ellipse", {
  expect_snap_plot("ellipse with from list",
    ggpm + geom_magnify(from = c(-110, -100, 35, 45),
                        to = c(-125, -105, 10, 30),
                        aspect = "fixed",
                        shape = "ellipse",
                        expand = 0)
  )

  expect_snap_plot("ellipse with from aes",
    ggpm + geom_magnify(aes(from = ID == "texas"),
                        to = c(-125, -105, 10, 30),
                        aspect = "fixed",
                        shape = "ellipse",
                        expand = 0)
  )
})


test_that("outline", {
  skip("Not working with s2...")
  expect_snap_plot("outline with from list",
    ggpm + geom_magnify(from = c(-110, -100, 35, 45),
                        to = c(-125, -105, 10, 30),
                        aspect = "fixed",
                        shape = "outline",
                        expand = 0)
  )

  expect_snap_plot("outline with from aes",
    ggpm + geom_magnify(aes(from = ID == "texas"),
                        to = c(-125, -105, 10, 30),
                        aspect = "fixed",
                        shape = "outline",
                        expand = 0)
  )
})


test_that("multiple", {
  expect_snap_plot("multiple outline",
    ggpm + geom_magnify(aes(from = ID %in% c("texas", "california")),
                        to = c(-125, -105, 10, 30),
                        aspect = "fixed",
                        shape = "outline",
                        expand = 0)
  )

  expect_snap_plot("multiple outline multiple proj",
    ggpm + geom_magnify(aes(from = ID %in% c("texas", "california")),
                        to = c(-125, -105, 10, 30),
                        aspect = "fixed",
                        shape = "outline", proj.combine = FALSE,
                        expand = 0)
  )

  expect_snap_plot("multiple outline multiple proj single",
    ggpm + geom_magnify(aes(from = ID %in% c("texas", "california")),
                        to = c(-125, -105, 10, 30),
                        aspect = "fixed",
                        shape = "outline", proj.combine = FALSE,
                        proj = "single",
                        expand = 0)
  )

  expect_snap_plot("multiple outline rect",
    ggpm + geom_magnify(aes(from = ID %in% c("texas", "california")),
                        to = c(-125, -105, 10, 30),
                        aspect = "fixed",
                        shape = "rect",
                        expand = 0)
  )

  expect_snap_plot("multiple outline ellipse",
    ggpm + geom_magnify(aes(from = ID %in% c("texas", "california")),
                        to = c(-125, -105, 10, 30),
                        aspect = "fixed",
                        shape = "ellipse",
                        expand = 0)
  )
})

