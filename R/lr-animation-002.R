# ------------------------------
# linear-regression-anim-002.gif
# ------------------------------
# Show how gradient affects a linear regression line

# Source setup script
source(here::here("R", "lr-animation-setup.R"))

# Create base plot with points and LR
base_plot <- ggplot(penguin) +
  aes(x = flipper_len, y = body_mass) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

# Create random data
random_df <- tibble(x = c(1:25), y = x + 2*rnorm(25))
random_df

# Create list of tables of gradient + intercepts
regs <- list()

# Changing intercepts, constant slope
regs$const_slope <- tibble(id = 1:25, slope = 1, intercept = -1:23)

# Changing slope, constant intercept
regs$const_intercept <- tibble(id=1:10, slope = -4:5, intercept = 2.5)

# Plot changing intercept
anim_changing_intercept <- ggplot(
  regs$const_slope %>% 
    # randomise order of intercepts
    slice(sample(1:n())) %>%
    mutate(id = factor(id, levels = id))
) +
  geom_point(data = random_df, aes(x, y)) + 
  geom_abline(aes(slope = slope, intercept = intercept)) +
  labs(x = "X", y = "Y") +
  ggtitle(
    label = "Comparing intercepts", 
    subtitle = "Slope (Gradient, Coefficient) is constant, intercept changes"
  ) +
  gganimate::transition_states(id, transition_length = 8, state_length = 8) +
  gganimate::ease_aes(slope = "elastic-in-out")

# Plot changing slope
anim_changing_slope <- ggplot(regs$const_intercept) +
  geom_point(data = random_df, aes(x, y)) + 
  geom_abline(aes(slope = slope, intercept = intercept)) +
  labs(x = "X", y = "Y") +
  ggtitle(
    label = "Comparing gradients", 
    subtitle = paste(
      "Slope (Gradient, Coefficient) Changes. Intercept is fixed.",
      "<br>Current Slope: **{regs$const_intercept %>% ",
      "filter(id == {closest_state}) %>% pull(.data$slope)}**"
    )
  ) +
  theme(plot.subtitle = ggtext::element_markdown()) +
  gganimate::transition_states(id, transition_length = 8, state_length = 8) +
  gganimate::ease_aes(slope = "quadratic-in-out")

# Render intercept gif
intercept_file <- "linear-regression-anim-002-changing-intercept.gif"
intercept_path <- here::here("Rmd", "linear-regression", "images", intercept_file)

gganimate::animate(
  anim_changing_intercept,
  device = "ragg_png",
  fps = 30,
  nframes = 800,
  width = 8,
  height = 8,
  units = "in",
  res = 120,
  renderer = gganimate::gifski_renderer(file = intercept_path)
)

# Render slope gif
slope_file <- "linear-regression-anim-002-changing-slope.gif"
slope_path <- here::here("Rmd", "linear-regression", "images", slope_file)

gganimate::animate(
  anim_changing_slope,
  device = "ragg_png",
  fps = 30,
  nframes = 800,
  width = 8,
  height = 8,
  units = "in",
  res = 120,
  renderer = gganimate::gifski_renderer(file = slope_path)
)
