
library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species))

xlim <- c(3, 3.5)
ylim <- c(6, 7)
inset_xlim <- c(2, 3)
inset_ylim <- c(4.5, 5.5)


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
  ggp <- ggp + geom_smooth(method = "lm", formula = y ~ x)
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


test_that("geom_boxplot", {
  ggp <- ggplot(iris, aes(Species, Sepal.Length, color = Species)) +
    geom_boxplot()
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
    geom_violin()
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



test_that("geom_density", {
  ggp <- ggplot(iris, aes(Sepal.Width, color = Species)) +
    geom_density()
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
