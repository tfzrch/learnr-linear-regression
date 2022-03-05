# Packages
library(tidyverse)
library(yaml)

# Read in palette from YAML
flatly <- read_yaml(here::here("inst", "extdata", "flatly.yml")) %>% 
  map(flatten_df) %>% 
  map(pivot_longer, cols = everything())

# Map the categorical names to their values in `grey` and `color` palettes
flatly$cat$value <- map_int(
  flatly$cat$value, 
  match, 
  table = c(flatly$col$name, flatly$grey$name)
)

flatly$cat$value <- c(flatly$col$value, flatly$grey$value)[flatly$cat$value]

# Bind to single df
flatly <- bind_rows(flatly)

# Write palette
write_rds(flatly, file = "data/flatly.rds")

# ggplot
ggplot(flatly %>% mutate(name = factor(name, levels = flatly$name))) +
  aes(x = name, ymin = 0, ymax = 1) +
  geom_linerange(col = flatly$value, size = 5) +
  coord_flip()
