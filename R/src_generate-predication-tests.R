#' Generate feature values within training range to predict with
#' 
#' @param n integer; the number of values to generate
#' @param feature unquoted tidyselect variable; the feature name in `data`
#' @param data data frame; the data to select `feature` from
#' 
#' @return A data frame containing one column; name matching `feature` and
#'  containing values randomly selected from values between the minimum and 
#'  maximum of `feature`
#'  
gen_x_predict <- function(n, feature, data = penguin) {
  data <- data %>% drop_na({{feature}})
  min <- data %>% pull({{feature}}) %>% min()
  max <- data %>% pull({{feature}}) %>% max()
  x <- sample(min:max, size = n, replace = TRUE)
  tibble({{feature}} := x)
}
