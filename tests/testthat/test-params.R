
library(ggplot2)

ggp <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point()

xlim <- c(3, 3.5)
ylim <- c(6,7)
inset_xlim <- c(2.8, 3.6)
inset_ylim <- c(4, 5.4)

test_param <- function (name, ...) {
  test_that(name, {
    expect_silent(
      ggm <- ggmagnify(ggp,
              xlim = xlim, ylim = ylim,
              inset_xlim = inset_xlim, inset_ylim = inset_ylim, ...)
    )
    expect_silent(
      ggm <- ggm + labs(title = name)
    )
    skip_on_ci()
    filename <- paste0("test-params-", name, ".png")
    expect_snapshot_file(ggsave(filename, ggm, width = 5, height = 5))
  })
}

test_param("defaults")
test_param("zoom", zoom = 2)
test_param("border", border = FALSE)
test_param("target", target = FALSE)
test_param("proj-corresponding", proj = "corresponding")
test_param("proj-single", proj = "single")
test_param("proj-none", proj = "none")
test_param("shadow", shadow = TRUE)
test_param("axes", axes = TRUE)
test_param("margin-1", margin = 20)
test_param("margin-4", margin = c(0, 0, 10, 10))
test_param("margin-unit", margin = grid::unit(c(10, 10, 10, 10), "mm"))
test_param("linewidth", linewidth = 1.5)
test_param("linetype", linetype = 2)
test_param("colour", colour = "red")
test_param("alpha", alpha = 0.4)
test_param("inset_linewidth", inset_linewidth = 1.5)
test_param("inset_linetype", inset_linetype = 2)
test_param("inset_colour", inset_colour = "red")
test_param("inset_alpha", inset_alpha = 0.4)
test_param("proj_linewidth", proj_linewidth = 1.5)
test_param("proj_linetype", proj_linetype = 3)
test_param("proj_colour", proj_colour = "red")
test_param("proj_alpha", proj_alpha = 0.4)
test_param("target_linewidth", target_linewidth = 1.5)
test_param("target_linetype", target_linetype = 2)
test_param("target_colour", target_colour = "red")
test_param("target_alpha", target_alpha = 0.4)
test_param("shadow_args", shadow = TRUE,
          shadow_args = list(x_offset = -10, y_offset = -10, colour = "pink"))
test_param("inset_expand", inset_expand = TRUE)
test_param("blank", blank = inset_blanks("panel.grid", axes = FALSE))
