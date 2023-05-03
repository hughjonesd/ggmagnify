
library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point()

x <- 3.25
width <- .5
y <- 6.5
height <- 1

to_x <- 3.2
to_y <- 5
magnify <- c(1.6, 1.4)

test_param <- function (name, ...) {
  test_that(name, {
    expect_silent(
      ggm <- ggp +
        labs(title = name) +
        geom_magnify_tile(x = x, width = width, y = y, height = height,
                     to_x = to_x, to_y = to_y, magnify = magnify, ...)
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
test_param("recompute", recompute = TRUE)


skip_if_not_installed("ggfx")
test_param("shadow", shadow = TRUE)
test_param("shadow.args", shadow = TRUE,
           shadow.args = list(x_offset = -10, y_offset = -10, colour = "pink"))

skip_if_not_installed("ggforce")
test_param("shape-ellipse", shape = "ellipse")

file.remove(list.files(pattern = "test-params.*.png"))
