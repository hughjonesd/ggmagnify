
skip_on_ci()
library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species))
ggp1d <- ggplot(iris, aes(Sepal.Width, color = Species, fill = Species))

xlim <- c(3, 3.5)
ylim <- c(6.5, 7.5)
inset_xlim <- c(2.2, 3.2)
inset_ylim <- c(4.2, 6.2)


test_that("geom_point", {
  ggp <- ggp + geom_point()
  expect_silent(
    ggm <- ggmagnify(ggp, xlim = xlim, ylim = ylim,
                     inset_xlim = inset_xlim, inset_ylim = inset_ylim)
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_point.png", ggm, width = 7, height = 7)
  )
})


test_that("geom_smooth", {
  ggp <- ggp + geom_point() + geom_smooth(method = "lm", formula = y ~ x)
  expect_silent(
    ggm <- ggmagnify(ggp, xlim = xlim, ylim = ylim,
                     inset_xlim = inset_xlim, inset_ylim = inset_ylim)
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_smooth.png", ggm, width = 7, height = 7)
  )
})


test_that("geom_line", {
  ggp <- ggp + geom_line(aes(group = Species))
  expect_silent(
    ggm <- ggmagnify(ggp, xlim = xlim, ylim = ylim,
                     inset_xlim = inset_xlim, inset_ylim = inset_ylim)
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_line.png", ggm, width = 7, height = 7)
  )
})


test_that("geom_label", {
  ggp <- ggp + geom_point() +
         geom_label(data = iris[seq(1, 101, 10),], aes(label = Species))
  expect_silent(
    ggm <- ggmagnify(ggp, xlim = xlim, ylim = ylim,
                     inset_xlim = inset_xlim + 2, inset_ylim = inset_ylim)
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_label.png", ggm, width = 7, height = 7)
  )
})


test_that("geom_hex", {
  skip_if_not_installed("hexbin")
  ggp <- ggp + geom_hex(bins = 10) + geom_point()
  expect_silent(
    ggm <- ggmagnify(ggp, xlim = xlim, ylim = ylim,
                     inset_xlim = inset_xlim, inset_ylim = inset_ylim)
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_hex.png", ggm, width = 7, height = 7)
  )
})


test_that("geom_rug", {
  ggp <- ggp + geom_rug() + geom_point()
  expect_silent(
    ggm <- ggmagnify(ggp, xlim = xlim, ylim = ylim,
                     inset_xlim = inset_xlim, inset_ylim = inset_ylim,
                     axes = TRUE)
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_rug.png", ggm, width = 7, height = 7)
  )
})


test_that("geom_density2d", {
  ggp <- ggp + geom_point() + geom_density2d()
  expect_silent(
    ggm <- ggmagnify(ggp, xlim = xlim, ylim = ylim,
                     inset_xlim = inset_xlim, inset_ylim = inset_ylim)
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_density2d.png", ggm, width = 7, height = 7)
  )
})


test_that("geom_tile", {
  ggp <- ggp + geom_tile(aes(fill = Species))
  expect_silent(
    ggm <- ggmagnify(ggp, xlim = xlim, ylim = ylim,
                     inset_xlim = inset_xlim, inset_ylim = inset_ylim)
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_tile.png", ggm, width = 7, height = 7)
  )
})


test_that("geom_density2d_filled", {
  ggp <- ggp + geom_density2d_filled()
  expect_silent(
    ggm <- ggmagnify(ggp, xlim = xlim, ylim = ylim,
                     inset_xlim = inset_xlim, inset_ylim = inset_ylim)
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_density2d_filled.png", ggm, width = 7, height = 7)
  )
})


test_that("geom_boxplot", {
  ggp <- ggplot(iris, aes(Species, Sepal.Length, color = Species)) +
    geom_point() + geom_boxplot()
  expect_silent(
    ggm <- ggmagnify(ggp, xlim = c(1.75, 2.25), ylim = ylim,
                     inset_xlim = c(0.5, 1.5), inset_ylim = c(7, 8))
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_boxplot.png", ggm, width = 7, height = 7)
  )
})


test_that("geom_violin", {
  ggp <- ggplot(iris, aes(Species, Sepal.Length, color = Species)) +
    geom_point() + geom_violin(fill = NA)
  expect_silent(
    ggm <- ggmagnify(ggp, xlim = c(1.75, 2.25), ylim = ylim,
                     inset_xlim = c(0.5, 1.5), inset_ylim = c(7, 8))
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_violin.png", ggm, width = 7, height = 7)
  )
})


test_that("geom_histogram", {
  ggp <- ggp1d + geom_histogram(bins = 10)
  expect_silent(
    ggm <- ggmagnify(ggp, xlim = xlim, ylim = c(10, 15),
                     inset_xlim = inset_xlim, inset_ylim = c(20, 30))
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_histogram.png", ggm, width = 7, height = 7)
  )
})


test_that("geom_density", {
  ggp <- ggp1d + geom_density()
  expect_silent(
    ggm <- ggmagnify(ggp, xlim = xlim, ylim = c(.4, .6),
                     inset_xlim = inset_xlim, inset_ylim = c(.5, 1))
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_density.png", ggm, width = 7, height = 7)
  )
})


test_that("geom_dotplot", {
  ggp <- ggp1d + geom_dotplot(binwidth = 0.2)
  expect_silent(
    ggm <- ggmagnify(ggp, xlim = xlim, ylim = c(0, 0.1),
                     inset_xlim = inset_xlim, inset_ylim = c(0.1, 0.3))
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_dotplot.png", ggm, width = 7, height = 7)
  )
})


test_that("geom_function", {
  # for fun
  ggp <- ggplot() +
    geom_function(aes(color = after_stat(y)), n = 1001,
                  fun = function(x) sin(1/x)) +
    xlim(0.02,1) +
    scale_color_gradient(low = "red", high = "blue")

  expect_silent(
    ggm <- ggmagnify(ggp, xlim = c(0.02, 0.12), ylim = c(-0.1, 0.1),
                     inset_xlim = c(.5, 1), inset_ylim = c(-1, 0))
  )
  expect_silent(
    print(ggm)
  )
  expect_snapshot_file(
    ggsave("test-geom_function.png", ggm, width = 7, height = 7)
  )
})





detritus <- list.files(pattern = "test-geom.*png")
file.remove(detritus)

