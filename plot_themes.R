#theme

theme_liniowy =   theme(
  axis.text.x = element_text(
    angle = 45,
    colour = 'grey29',
    family = 'Lato bold',
    size = 8,
    hjust = 1
  ),
  axis.text.y = element_text(
    colour = 'grey29',
    family = 'Lato bold',
    size = 10,
    hjust = 1
  ),
  axis.title.x = element_text(
    colour = 'grey29',
    family = 'Lato bold',
    size = 12,
    hjust = 0.5
  ),
  axis.title.y = element_text(    
    colour = 'grey29',
    family = 'Lato bold',
    size = 12,
    hjust = 0.5),
  plot.title = element_text(
    colour = 'grey29',
    hjust = 0.5,
    family = 'Lato bold',
    size = 20
  ),
  axis.ticks.x = element_blank(),
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_line(colour = "grey90", size = 0.2),
  plot.caption = element_text(
    size = 14,
    family = 'Lato light',
    colour = 'grey20'
  ),
  legend.position = 'bottom',
  legend.title = element_blank(),
  legend.text = element_text(
    size = 16,
    family = 'Lato',
    color = 'grey29'
  ),
  strip.background = element_blank(),
  strip.text = element_text(size = 20,
                            colour = 'grey29',
                            family = 'Lato bold'
  ),
  plot.subtitle = element_text(colour = 'grey29',
                               hjust = 0.5,
                               family = 'Lato bold',
                               size = 15)
)

theme_barowy = theme_liniowy +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(
          angle = 0,
          hjust = 0.5,
          size = 10,
          family = 'Lato bold'
        ),
        axis.text.y = element_text(
          angle = 0,
          hjust = 1,
          size = 10,
          family = 'Lato bold'
        ),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,
                                  colour = 'grey29',
                                  family = 'Lato bold'
        ),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white"),
        # axis.text.x = element_blank(),
        plot.subtitle = element_text(
          colour = 'grey29',
          hjust = 0.5,
          family = 'Lato bold',
          size = 15))

theme_barowy_siatka = theme_barowy +
  theme(axis.ticks.x = element_line(color = 'black'),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey80", size = 0.3),
        axis.text.y = element_text(
          hjust = 1,
          size = 12)
  )
