

library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species))
ggp1d <- ggplot(iris, aes(Sepal.Width, color = Species, fill = Species))


test_plot_type <- function(
                    name,
                    plot,
                    x = 3.25,
                    width = 0.5,
                    y = 7,
                    height = 1,
                    to_x = 2.7,
                    to_y = 5.2,
                    to_width = 0.75,
                    to_height = 1.5,
                    ...) {
  test_that(name, {
    expect_silent(
      ggm <- plot + geom_magnify_tile(x = x, width = width, y = y, height = height,
                       to_x = to_x, to_y = to_y, to_width = to_width,
                       to_height = to_height, ...)
    )
    expect_silent(
      print(ggm)
    )
    skip_on_ci()
    expect_snapshot_file(
      ggsave(sprintf("test-%s.png", name), ggm, width = 4, height = 4)
    )
  })
}

test_plot_type("geom_point", ggp + geom_point())
test_plot_type("geom_smooth",
               ggp + geom_point()+ geom_smooth(method = "lm", formula = y ~ x))
test_plot_type("geom_line", ggp + geom_line(aes(group = Species)))
test_plot_type("geom_label", ggp + geom_point() +
               geom_label(data = iris[seq(1, 101, 10),], aes(label = Species)))
test_plot_type("geom_rug", ggp + geom_rug() + geom_point())
test_plot_type("geom_density2d", ggp + geom_point() + geom_density2d())
test_plot_type("geom_tile", ggp + geom_tile(aes(fill = Species)))
test_plot_type("geom_density2d_filled", ggp + geom_density2d_filled())

test_plot_type("geom_boxplot",
  ggplot(iris, aes(Species, Sepal.Length, color = Species)) +
  geom_point() + geom_boxplot())
test_plot_type("geom_violin",
  ggplot(iris, aes(Species, Sepal.Length, color = Species)) +
  geom_point() + geom_violin(fill = NA))

test_plot_type("geom_histogram", ggp1d + geom_histogram(bins = 10),
               height = 6, to_height = 9, to_y = 20)
test_plot_type("geom_density", ggp1d + geom_density(), x = 3.25, width = 0.5,
               y = 1, height = 0.2, to_y = 0.5, to_x = 2.7, to_width = 1,
               to_height = 0.4)

test_plot_type("geom_dotplot", ggp1d + geom_dotplot(binwidth = 0.2),
               y = 2, to_y = 1)

test_plot_type("geom_function",
  # for fun. `data` is required
  ggplot(data = data.frame(dummy = 1)) +
    geom_function(aes(color = after_stat(y)), n = 5001,
                  fun = function(x) suppressWarnings(sin(1/x)), na.rm = TRUE) +
    coord_cartesian(xlim = c(0.02,1)) +
    scale_color_gradient(low = "red", high = "blue"),
  x = 0.02, width = 0.04, y = -0.5, height = 1, to_x = 0.75, to_y = -0.5,
  to_width = 0.4, to_height = 1
)

skip_if_not_installed("hexbin")
test_plot_type("geom_hex", ggp + geom_hex(bins = 10) + geom_point())


detritus <- list.files(pattern = "test-geom.*png")
file.remove(detritus)
