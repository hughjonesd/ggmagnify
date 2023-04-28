
# Advanced usage

library(ggplot2)

booms <- ggplot(faithfuld, aes(waiting, eruptions)) +
         geom_contour_filled(aes(z = density)) +
         scale_fill_viridis_d(option = "B")

shadow_args <- list(
  colour = alpha("grey80", 0.8),
  x_offset = 0,
  y_offset = 0,
  sigma = 10
)

ggm <- ggmagnify(booms,
                 xlim = c(80, 92), ylim = c(4, 4.8),
                 inset_xlim = c(70, 94), inset_ylim = c(1.7, 3.3),
                 shadow = TRUE, shadow_args = shadow_args,
                 colour = "white")

# modify the inset only:
ggm$inset <- ggm$inset +
             geom_point(data = faithful, color = "red", fill = "white",
                        alpha = 0.7, size = 2, shape = "circle filled")

ggm

# modify the original plot only:

ggm$plot <- ggm$plot + scale_fill_grey()

ggm

# modify both:

ggm + scale_fill_viridis_d(option = "C")

