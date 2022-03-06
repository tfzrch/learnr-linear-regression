#' Generate data to demonstrate the Pearson's Correlation Coefficient
#' 
#' @param seed integer; passed on to `set.seet()`
#' 
#' @return data frame; randomly generated data with x and y coordinates,
#'  grouped by a `type` of correlation and with an associated *r* value.
#'  
#'  
gen_pearson_demo <- function(seed = 101) {
  
  # Set Seed
  set.seed(seed)
  
  # Generate data
  data <- tibble(
    x = seq(1, 30, by = 2), 
    perfect = seq(1, 30, by = 2), 
    strong = perfect + 1*rnorm(15, sd = 6),
    weak = perfect + 1 * rnorm(15, sd = 20),
  )
  
  # Reshape to add negative equivalents
  data <- data %>% 
    pivot_longer(cols = -x, values_to = "pos", names_to = "cor") %>% 
    mutate(neg = -pos)
  
  # Reshape so negatives and positives are in rows
  data <- data %>% 
    pivot_longer(cols = c(pos, neg), names_to = "dir", values_to = "y")
  
  # Format labelling
  data <- data %>% 
    mutate(
      dir = if_else(dir == "pos", "Positive", "Negative"), 
      cor = cor %>% str_replace("_", " ") %>% str_to_title()
    ) %>% 
    mutate(type = forcats::fct_cross(cor, dir, sep = "\n")) %>% 
    mutate(type = forcats::lvls_reorder(type, idx = c(1, 2, 3, 6, 5, 4)))
  
  # Add the coefficient
  data <- data %>% 
    group_by(type) %>%
    mutate(pearson = cor(x, y)) %>% 
    ungroup()
}
