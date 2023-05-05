
library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point()

x <- 3.25
width <- 0.5
y <- 6.5
height <- 1

to_x <- 3.2
to_y <- 5
to_width <- 0.8
to_height <- 1.4

test_param <- function (name, ..., .ggplot = ggp) {
  if (grepl("\\.", name)) warning("Dot in snapshot file name may cause testthat bug.")
  test_that(name, {
    expect_silent(
      ggm <- .ggplot +
        labs(title = name) +
        geom_magnify_tile(x = x, width = width, y = y, height = height,
                     to_x = to_x, to_y = to_y, to_width = to_width,
                     to_height = to_height, ...)
    )
    expect_silent(
      print(ggm)
    )
    skip_on_ci()
    filename <- paste0("test-params-", name, ".png")
    expect_snapshot_file(ggsave(filename, ggm, width = 5, height = 5))
  })
}

test_param("defaults")
test_param("expand", expand = FALSE)
test_param("axes", axes = "xy")
test_param("axes-x", axes = "x")
test_param("axes-y", axes = "y")
test_param("proj-corresponding", proj = "corresponding")
test_param("proj-single", proj = "single")
test_param("linetype", linetype = 2)
# these are dashes, not dots, to get round a testthat bug
test_param("target-linetype", target.linetype = 2)
test_param("inset-linetype", inset.linetype = 2)
test_param("proj-linetype", proj.linetype = 1)
test_param("alpha", alpha = 0.5, colour = "red")
test_param("linewidth", linewidth = 1.5)
test_param("linetype", linetype = 2)
test_param("plot", plot = ggp + geom_density2d())
test_param("recompute", recompute = TRUE, .ggplot = ggp + geom_density_2d())
test_param("scale-inset", scale.inset = c(2, 0.5))



skip_if_not_installed("ggfx")
test_param("shadow", shadow = TRUE)
test_param("shadow-args", shadow = TRUE,
           shadow.args = list(x_offset = -10, y_offset = -10, colour = "pink"))

mask <- grid::polygonGrob(x = c(0, 0.5, 1, 0.5, 0.2), y = c(0, 0.1, 0.5, 1, 0.3))
test_param("shape-mask", shape = mask)

skip_if_not_installed("ggforce")
test_param("shape-ellipse", shape = "ellipse")
test_param("shadow-ellipse", shadow = TRUE, shape = "ellipse")
test_param("proj-single-ellipse", proj = "single", shape = "ellipse")
test_param("scale-inset-ellipse", scale.inset = c(2, 0.5), shape = "ellipse")
test_param("recompute-ellipse", recompute = TRUE, shape = "ellipse",
           .ggplot = ggp + geom_density_2d())

file.remove(list.files(pattern = "test-params.*.png"))
