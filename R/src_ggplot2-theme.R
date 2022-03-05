#' Custom theme for ggplot2
#' 
#' @param pal data frame; the palette to use for colours
#' 
#' @return a ggplot2 theme
#' 
ggtheme <- function() {
  theme_classic() +
    theme(
      axis.title = element_text(face = "bold", color = get_pal("primary")),
      axis.line = element_line(color = get_pal("primary")),
      axis.ticks = element_line(color = get_pal("primary")),
      plot.title = element_text(face = "bold", color = get_pal("primary")),
      plot.background = element_rect(fill = get_pal("light")),
      panel.background = element_rect(fill = get_pal("light"))
    )
}

#' Get a colour from the flatly palette
#' 
#' @param name string; the colour to retrieve
#' @param pal data frame; the flatly palette
#' 
get_pal <- function(color, pal = here::here("data", "flatly.rds")) {
  pal <- read_rds(pal)
  filter(pal, .data[["name"]] == color) %>% 
    pull(.data[["value"]])
}
