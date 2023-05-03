
library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species, shape = Species))
ggp2 <- ggp + geom_point()

from <- c(3, 6, 3.5, 7)
to <- c(2.4, 4.3, 3.2, 5.7)

test_that("default", {
  expect_silent(
    ggp2 +
      scale_x_continuous(limits = c(2, 5)) +
      geom_magnify(from = from, to = to)
  )

  expect_silent(
    ggp2 +
      scale_y_continuous(limits = c(4, 7)) +
      geom_magnify(from = from, to = to)
  )
})


test_that("reverse", {
  skip("Not working yet")
  expect_silent(
    ggp2 +
      scale_x_reverse() +
      geom_magnify(from = from, to = to)
  )
})


test_that("date", {
  skip("Not working yet")
  ggpd <- ggplot(economics, aes(date, unemploy/pop)) +
    geom_line() +
    scale_x_date()

  ggpd + geom_magnify(from = list(as.Date("1970-01-01"), 0,
                                  as.Date("1972-01-01"), 0.03),
                      to = list(as.Date("1990-01-01"), 0,
                                  as.Date("2010-01-01"), 0.03))
})


test_that("binned", {
  expect_silent(
    ggp2 +
      scale_x_binned(breaks = seq(2, 5, 0.5)) +
      geom_magnify(from = c(2.5, 6.0, 3.5, 7.0), to = to)
  )
})

test_that("log", {
  skip("Not working yet")
  expect_silent(
    ggp2 +
      scale_x_log10() +
      scale_y_log10() +
      geom_magnify(from = from, to = to)
  )

  expect_snapshot_file(
    ggsave("test-scales-log.png", width = 5, height = 5)
  )
})
