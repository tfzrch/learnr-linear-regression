# Packages
library(tidyverse)

# Source
source(here::here("R", "pearson-r-image-src.R"))
source(here::here("R", "src_ggplot2-theme.R"))

# Theme
theme_set(ggtheme())

# Demo data
demo <- gen_pearson_demo()

# Glue string for r labels
glue_str <- "r = {round(pearson, digits = 1)}"

# Plot
plot <- ggplot(demo, aes(x, y, col = pearson)) +
  geom_point() +
  geom_text(
    mapping = aes(x = 15, y = 40, label = glue::glue(glue_str)), 
    col = get_pal("primary")
  ) +
  scale_color_distiller(type = "div", guide = "none") +
  coord_equal() +
  facet_wrap(~type, nrow = 1) +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(
      color = get_pal("primary"), 
      fill = "transparent",
      size = .05,
    ),
    strip.text = element_text(
      face = "bold", 
      size = 12, 
      color = get_pal("secondary")
    )
  )

ragg::agg_png(
  here::here("Rmd", "linear-regression", "images", "pearson.png"),
  width = 8,
  height = 6,
  units = "in",
  res = 120
)
plot
dev.off()
