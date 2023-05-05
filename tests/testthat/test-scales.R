
library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species, shape = Species))
ggp2 <- ggp + geom_point()

from <- c(3, 6, 3.5, 7)
to <- c(2.4, 4.3, 3.2, 5.7)

test_that("limits", {
  ggp_limits_x <- ggp2 +
      scale_x_continuous(limits = c(2, 5)) +
      geom_magnify(from = from, to = to)
  expect_silent(
    print(ggp_limits_x)
  )


  ggp_limits_y <- ggp2 +
    scale_y_continuous(limits = c(4, 8)) +
    geom_magnify(from = from, to = to)

  expect_silent(
    print(ggp_limits_y)
  )

  skip_on_ci()
  expect_snapshot_file(
    ggsave("test-scales-limits-x.png", ggp_limits_x, width = 5, height = 5)
  )
  expect_snapshot_file(
    ggsave("test-scales-limits-y.png", ggp_limits_y, width = 5, height = 5)
  )
})


test_that("reverse", {
  ggp_reverse_x <- ggp2 +
    scale_x_reverse() +
    geom_magnify(from = from[c(3,2,1,4)], to = to[c(3,2,1,4)])

  expect_silent(
    print(ggp_reverse_x)
  )

  ggp_reverse_y <- ggp2 +
    scale_y_reverse() +
    geom_magnify(from = from[c(1,4,3,2)], to = to[c(1,4,3,2)])

  expect_silent(
    print(ggp_reverse_y)
  )

  skip_on_ci()
  expect_snapshot_file(
    ggsave("test-scales-reverse-x.png", ggp_reverse_x, width = 5, height = 5)
  )

  expect_snapshot_file(
    ggsave("test-scales-reverse-y.png", ggp_reverse_y, width = 5, height = 5)
  )
})


test_that("reversed coords", {

  ggp_rev_x <- ggp2 + coord_cartesian(xlim = c(6, 2)) +
    geom_magnify(from = from, to = to)

  expect_no_error(
    print(ggp_rev_x)
  )

  ggp_rev_y <- ggp2 +
    coord_cartesian(ylim = c(8, 4)) +
    geom_magnify(from = from, to = to)

  expect_no_error(
    print(ggp_rev_y)
  )

  skip_on_ci()
  expect_snapshot_file(
    ggsave("test-scales-reversed-coords-x.png", ggp_rev_x, width = 5, height = 5)
  )
  expect_snapshot_file(
    ggsave("test-scales-reversed-coords-y.png", ggp_rev_y, width = 5, height = 5)
  )
})



test_that("log", {
  dfr <- data.frame(x = c(1:10, 60), y = c(1:10, 60))
  ggp_log <- ggplot(dfr, aes(x = x, y = y)) + geom_point()

  ggp_log_x <- ggp_log +
    scale_x_log10() +
    geom_magnify(from = c(1, 1, 10, 10), to = c(15, 35, 35, 55))

  expect_silent(
    print(ggp_log_x)
  )

  ggp_log_xy <- ggp_log +
    scale_x_log10() +
    scale_y_log10() +
    geom_magnify(from = c(1, 1, 10, 10), to = c(15, 35, 35, 55))

  expect_silent(
    print(ggp_log_xy)
  )

  skip_on_ci()
  expect_snapshot_file(
    ggsave("test-scales-log-x.png", ggp_log_x, width = 5, height = 5)
  )
  expect_snapshot_file(
    ggsave("test-scales-log-xy.png", ggp_log_xy, width = 5, height = 5)
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
  skip("Not working yet")
  expect_silent(
    ggp2 +
      scale_x_binned(breaks = seq(2, 5, 0.5)) +
      geom_magnify(from = c(2.7, 6.0, 3.3, 7.0), to = to)
  )
})

file.remove(list.files(pattern = "test-scales.*.png"))
