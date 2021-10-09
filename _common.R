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

# default
theme_set(theme_grey())

# theme_td

theme_td <- function() {
  ggthemes::theme_clean(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "grey95"),
      strip.placement = "outside",
      strip.text = element_text(hjust = 0), 
      # panel.background = element_rect(fill = "grey98"),
      panel.spacing = unit(3, units = "mm")
    ) 
}

update_geom_defaults("label", list(hjust = "inward", size = 3))
update_geom_defaults("col", list(fill = "#007f7f"))

sociedad_respeta %>% 
  ggplot(aes(porcentaje, p501)) +
  geom_col() +
  geom_label(aes(label = round(porcentaje, 1)))+
  labs(title = "Respeto a la orientación sexual/identidad de género", 
       caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "Percepción de personas LGBTI") +
  theme_td() 

respeta_reconoce %>% 
  ggplot(aes(porcentaje, p504)) +
  geom_col() +
  facet_grid(rows = "p501")+
  geom_label(aes(label = round(porcentaje, 1)))+
  labs(caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "Recocimiento de derechos humanos en personas LGBTI") +
  theme_td()
