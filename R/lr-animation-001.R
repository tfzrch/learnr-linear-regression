# ------------------------------
# linear-regression-anim-001.gif
# ------------------------------
# Show how predictions can be made from a LR by reading x > model > y on a
# scatter plot

# Source setup script
source(here::here("R", "lr-animation-setup.R"))

# Generate 10 examples of flipper_len to predict body_mass for
x_predict <- gen_x_predict(10, flipper_len)

# Add the predictions
x_predictions <- x_predict %>%
  mutate(body_mass = predict_lm_penguin_fl_bm(newdata = x_predict))

# Add an id, and text for the labels to show on the plot
x_predictions <- x_predictions %>% 
  rowid_to_column() %>% 
  mutate(label = make_label(flipper_len, body_mass))

# Calculate coordinates for the labels
position_lab_penguin <- partial(position_label, train = penguin) # for readability

x_predictions <- x_predictions %>% 
  rowwise() %>% 
  mutate(
    label_x = flipper_len %>% position_lab_penguin(feature = flipper_len),
    label_y = body_mass %>% position_lab_penguin(feature = body_mass)
  ) %>% 
  ungroup()

# Create a base plot to extract the x- and y-axis minimum values from
base_plot <- ggplot(x_predictions) +
  geom_point(
    data = penguin, 
    mapping = aes(x=flipper_len, y = body_mass), 
    inherit.aes = F,
    col = get_pal("purple")
  ) +
  geom_smooth(
    data = penguin, 
    mapping = aes(x=flipper_len, y = body_mass), 
    inherit.aes = F, 
    col = get_pal("pink"),
    method = "lm"
  )

# Use the plot to add the segment intercepts
x_predictions <- x_predictions %>% 
  mutate(
    intercept_x = position_segment_intercept(axis = "y", ggplot_build(base_plot)),
    intercept_y = position_segment_intercept(axis = "x", ggplot_build(base_plot))
  )

# Re-build the plot using updated `x_predictions`
plot <- ggplot_build(base_plot)$plot

# Add the segments
plot <- plot +
  geom_segment(
    data = x_predictions,
    aes(x = flipper_len, y = intercept_x, xend = flipper_len, yend = body_mass), 
    arrow = arrow(15, unit(2.5, "mm"), type = "closed"), size = 0.8, col = get_pal("orange")
  ) +
  geom_segment(
    data = x_predictions,
    aes(x = flipper_len, y = body_mass, xend = intercept_y, yend = body_mass),
    arrow = arrow(15, unit(2.5, "mm"), type = "closed"), size = 0.8, col = get_pal("orange")
  )

# Add the labels and titles
subtitle <- paste(
  "New Penguin! We know its Flipper Length is",
  "**{x_predictions %>% filter(rowid == closest_state)",
  "%>% pull(.data$flipper_len)}**.",
  "<br>What do we predict its Body Mass to be?"
)

plot <- plot +
  geom_label(aes(x = label_x, y = label_y, label = label)) +
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)") +
  ggtitle(
    "Predicting Penguin Body Mass with Flipper Length", 
    subtitle = subtitle
  ) +
  theme(plot.subtitle = ggtext::element_markdown())

# Animate
anim <- plot + 
  gganimate::transition_states(
    rowid,
    transition_length = 5,
    state_length = 10
  )

# Animation output path
anim_out <- here::here("Rmd", "linear-regression", "images", "linear-regression-anim-001.gif")

# Render
gganimate::animate(
  anim,
  device = "ragg_png",
  fps = 30,
  nframes = 800,
  width = 8,
  height = 8,
  units = "in",
  res = 120,
  renderer = gganimate::gifski_renderer(file = anim_out)
)
