#' Glue labels for linear regression animation
#' 
#' @param x the x-value for the label
#' @param y the y-value for the label
#' 
#' @return string; containing the labelled copies of `x` and `y`, indicating
#'  that `y` is a prediction
#' 
make_label <- function(x, y) {
  x_str <- glue::glue("x: {x}")
  y_str <- glue::glue("Predicted y: {round(y, digits=1)}")
  paste(x_str, y_str, sep = "\n")
}

#' Calculate ideal placement for labels in linear regression animation
#' 
#' @param pred numeric; the predicted value to be labelled
#' @param feature unquoted tidyselect variable; the feature name in `train`
#' @param train data frame; the data used to train the model
#' 
#' @return numeric; `pred` +/- a scaling factor calculated from the training
#'  data found for the `feature` in `train`.
#' 
position_label <- function(pred, feature, train) {
  train_feature <- train %>% drop_na({{feature}}) %>% pull({{feature}})
  train_midpoint <- mean(c(min(train_feature), max(train_feature)))
  scaling_factor <- mean(train_feature) / sd(train_feature)
  
  if (pred <= train_midpoint)  {
    out <- pred + 1 * abs(scaling_factor)
  } else if (pred > train_midpoint) {
    out <- pred - 1 * abs(scaling_factor)
  }
  # Return
  out
}

#' Calculate the xintercept or yintercept for segments in LR animation
#' 
#' @param axis string; `"x"` or `"y"`
#' @param plot the plot; should be the output of `ggplot_build()` applied to
#'  the plot
#'  
#' @return numeric; the intercept
#' 
position_segment_intercept <- function(axis, plot) {
  layout <- plot$layout
  panel <- glue::glue("panel_scales_{axis}")
  range <- layout[[panel]][[1]]$range$range
  intercept <- range[1]
  intercept
}
