# example R options set globally
options(width = 60)

# example chunk options set globally
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  echo = TRUE,
  warning = FALSE,
  message = FALSE
  )

# graficos tema en común

theme_td <- function() {
  ggthemes::theme_clean(base_size = 11) +
    ggplot2::theme(
      # panel.border = element_rect(color =  alpha("#007F7F", 0.25), fill = NA, linetype = 2),
      panel.spacing = ggplot2::unit(3, units = "mm"),
      strip.text = ggplot2::element_text(hjust = 0, face = "bold", colour = "#003F3F"), 
      strip.background = ggplot2::element_rect(fill = ggplot2::alpha("#007F7F", 0.25)),
      strip.placement = "inside",
      legend.position = "top",
      legend.background = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 9)
    ) 
}

# theme_td
ggplot2::theme_set(theme_td())

# update geoms
ggplot2::update_geom_defaults("label", list(hjust = "inward", size = 3))
ggplot2::update_geom_defaults("col", list(fill = "#009f9f"))
ggplot2::update_geom_defaults("line", list(size = 1.5, linetype = 1, color = ggplot2::alpha("#007F7F", 0.25)))

# default fill and color scale
scale_fill_brewer_d <- function(...) {
  ggplot2::scale_fill_brewer(palette = "Set2", ...)
}

scale_color_brewer_d <- function(...) {
  ggplot2::scale_color_brewer(palette = "Set2", ...)
}

options(
  ggplot2.discrete.fill = scale_fill_brewer_d,
  ggplot2.discrete.colour = scale_color_brewer_d
)
