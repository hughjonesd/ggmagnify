

library(ggplot2)

skip_if_not_installed("sf")
skip_if_not_installed("maps")
skip_on_ci()

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


test_that("geom_magnify viewports derive from the base plot", {
  to1 <- c(-125, -105, 20, 30)
  to2 <- c(-95, -75, 30, 45)

  inset_plot <- ggpm +
    geom_magnify(from = c(-115, -105, 30, 40), to = to1, expand = 0) +
    geom_magnify(from = c(-90, -80, 35, 45), to = to2, expand = 0)

  gt <- ggplotGrob(inset_plot)
  panel <- gt$grobs[[which(gt$layout$name == "panel")]]
  mg_children <- panel$childrenOrder[grepl("ggmagnify", panel$childrenOrder)]

  expect_length(mg_children, 2L)

  inset_viewports <- lapply(mg_children, function(nm) {
    vp <- panel$children[[nm]]$children[["layout"]]$vp
    c(
      x = grid::convertX(vp$x, "native", valueOnly = TRUE),
      y = grid::convertY(vp$y, "native", valueOnly = TRUE),
      width = grid::convertWidth(vp$width, "native", valueOnly = TRUE),
      height = grid::convertHeight(vp$height, "native", valueOnly = TRUE)
    )
  })

  base_build <- ggplot_build(ggpm)
  panel_params <- base_build$layout$panel_params[[1]]
  coord <- base_build$layout$coord

  expected_viewports <- lapply(list(to1, to2), function(to) {
    limits <- data.frame(x = c(to[1], to[2]), y = c(to[3], to[4]))
    limits_t <- coord$transform(limits, panel_params)
    x_rng <- range(limits_t$x, na.rm = TRUE)
    y_rng <- range(limits_t$y, na.rm = TRUE)
    c(x = mean(x_rng), y = mean(y_rng),
      width = diff(x_rng), height = diff(y_rng))
  })

  expect_equal(inset_viewports, expected_viewports, tolerance = 1e-6)
})

