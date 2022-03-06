# -------------------------------------
# linear-regression-anim- SETUP SCRIPT
# -------------------------------------

# Load packages
library(palmerpenguins)
library(tidyverse)

# Set seed
set.seed(1021)

# Source
list.files(
  here::here("R"), 
  pattern = "^src_|lr-animation-", 
  full.names = TRUE
) %>% 
  walk(source, .GlobalEnv)

# Theme
theme_set(ggtheme())

# Clean up penguins
penguin <- penguins %>% 
  rename_with(str_remove_all, pattern = "_mm$|_g$") %>% 
  rename_with(str_remove_all, pattern = "(?<=len)gth$") %>% 
  as_tibble() %>% 
  select(flipper_len, body_mass) %>% 
  drop_na()

# Setup linear model for illustrative plots
lm_penguin_fl_bm <- lm(body_mass ~ flipper_len, data = penguin)

# Setup predictor partial for plotting ease
predict_lm_penguin_fl_bm <- partial(predict, object = lm_penguin_fl_bm)