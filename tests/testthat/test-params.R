
library(ggplot2)


ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point()

from1 <- c(3, 3.5, 6, 7)
to1 <- c(2.4, 3.2, 4.3, 5.7)


test_param <- function (name, ..., from = from1, to = to1, .ggplot = ggp) {
  if (grepl("\\.", name)) warning("Dot in snapshot file name may cause testthat bug.")
  test_that(name, {
    expect_silent(
      ggm <- .ggplot +
        labs(title = name) +
        geom_magnify(from = from, to = to, ...)
    )
    expect_silent(
      print(ggm)
    )
    skip_on_ci()
    filename <- paste0("test-params-", name, ".png")
    expect_snapshot_file(ggsave(filename, ggm, width = 4, height = 4))
  })
}

test_param("defaults")
mask <- grid::polygonGrob(x = c(3, 3.5, 4, 3.5, 3.2),
                          y = c(6, 6.3, 6.7, 6.6, 6.8), default.units = "native")
test_param("from-mask", from = mask)
test_param("expand", expand = 0)
test_param("aspect-fixed", aspect = "fixed")
test_param("axes", axes = "xy")
test_param("axes-x", axes = "x")
test_param("axes-y", axes = "y")
test_param("corners", corners = 0.1)
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
test_param("plot", plot = ggp + geom_density2d() + inset_theme(axes = ""))
test_param("recompute", recompute = TRUE, .ggplot = ggp + geom_density_2d())
test_param("scale-inset", scale.inset = c(2, 0.5))
test_param("shape-ellipse", shape = "ellipse")
test_param("proj-single-ellipse", proj = "single", shape = "ellipse")
test_param("scale-inset-ellipse", scale.inset = c(2, 0.5), shape = "ellipse")
test_param("recompute-ellipse", recompute = TRUE, shape = "ellipse",
           .ggplot = ggp + geom_density_2d())

skip_if_not_installed("ggfx")
test_param("shadow", shadow = TRUE)
test_param("shadow-args", shadow = TRUE,
           shadow.args = list(x_offset = -10, y_offset = -10, colour = "pink"))
test_param("shadow-ellipse", shadow = TRUE, shape = "ellipse")

test_param("proj-fill", proj.fill = alpha("yellow", 0.2))

file.remove(list.files(pattern = "test-params.*.png"))
