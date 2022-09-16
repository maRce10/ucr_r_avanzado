
library(ggnewscale)
library(ggplot2)
# Equivalent to melt(volcano)
topography <- expand.grid(x = 1:nrow(volcano),
                          y = 1:ncol(volcano))
topography$z <- c(volcano)

# point measurements of something at a few locations
set.seed(42)
measurements <- data.frame(x = runif(30, 1, 80),
                           y = runif(30, 1, 60),
                           thing = rnorm(30))

ggplot(mapping = aes(x, y)) +
  geom_contour_filled(data = topography, aes(z = z, color = stat(level)), size =2, alpha = 0.6, show.legend = FALSE) +
  # Color scale for topography
  scale_color_viridis_d(option = "D", alpha = 0.1) +
  # geoms below will use another color scale
  new_scale_color() +
  geom_point(data = measurements, size = 4, aes(color = thing)) +
  # Color scale applied to geoms added after new_scale_color()
  scale_color_viridis_c(option = "A", alpha = 0.9) +
  labs(x = "", y = "", color = "") +
  theme_classic(base_size = 20)
