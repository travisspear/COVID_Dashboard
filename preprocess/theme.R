#### Colors ----
color_blue <- "#172b4d"
color_lightblue <- "#4298b5" 
color_grey <- "grey50"
color_red <- "#CF3C4D"
color_neutral <- "#9b74b0"
color_male <- color_lightblue
color_female <- color_neutral


#### ggplot Theme ----
theme_set(theme_minimal(base_size = 14))
theme_ts <- theme_minimal() + 
  theme(legend.position = "none",
        axis.text = element_text(face = "bold"),
        panel.background = element_blank()
        )
#### Plotly Options ----
plotly_options_ts <- function(p){
  p %>%
    config(displayModeBar = F) %>% 
    layout(dragmode = FALSE) %>%
    layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)")
           # fig_bgcolor   = "rgba(0, 0, 0, 0)")
}

