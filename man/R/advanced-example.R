
# Advanced usage

library(ggplot2)

ggp <- ggplot(diamonds, aes(carat, depth, color = cut)) + geom_point()

ggm <- ggmagnify(ggp, xlim = c(1.5, 2.5), ylim = c(60, 65),
          inset_xlim = c(2, 5), inset_ylim = c(40, 55),
          compose = FALSE)

# modify the inset like a ggplot object:
ggm$inset <- ggm$inset +
               theme_classic() +
               inset_theme(axes = TRUE) +
               theme(plot.margin = margin(10, 12, 10, 10))

compose(ggm, ggp)
