source("set_up.R")
source("styles.R")
source("functions.R")




#terms to search -------------------------------------------------------------

terms = c("Kobe Bryant",
          "Feminismo",
          "Coronavirus", 
          "Covid",
          "Dólar",
          "Zoom",
          "Google classroom",
          "López-Gatell",
          "5G",
          "Vacuna covid",
          "Huracán",
          "Donald Trump",
          "Joe Biden",
          "Maradona"
)

#download data from gtrends ---------------------------------------------------
data_google_list = map(terms,get_trend, geo="MX")


data_google = do.call(rbind, data_google_list) %>%
  mutate(term = factor(term,
                       levels  = terms,
                       ordered = T
  )
  )



# create plot ------------------------------------------------------------------
plot = ggplot(data = data_google,
       aes(x = date,
           y = hits,
           fill = term,
           color = term)
)+
  #'area ----------------------------------------------------------------------
  geom_area(
    alpha = .7
            ) +
  #'facet by term -------------------------------------------------------------
  facet_grid(
    term ~ ., switch = "y"
             ) +
  
  #'axis labels ----------------------------------------------------------------
  scale_x_date(
    date_breaks = "2 month",
    labels = function(x){str_to_title(lubridate::month(x, label = T, abbr = F))}
               ) +
  
  #'caption --------------------------------------------------------------------
  labs(
    caption = "**Data:** trends.google.com, diciembre 2020 | **Chart:** Andres Arau | **Inspired by:** Roshaan Khan"
    
  )+
  
  #'colors --------------------------------------------------------------------
  scale_fill_manual(values =  rep(pallete_google, 4)) +
  scale_color_manual(values =  rep(pallete_google, 4)) +
  #'theme ---------------------------------------------------------------------
  theme(
    #'Panel
    panel.background = element_blank(),
    #'axis
    axis.text.x = element_markdown(family="Open Sans", size = 14, face = 'bold', color = "#616466"),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    #'strip
    strip.text.y.left= element_markdown(angle=360, family = "Open Sans", hjust = 0, size = 14, 
                                        face = 'bold', color = "#616466"),
    strip.background = element_blank(),
    
    #'legend
    legend.position = 'none',
    
    #title 
    plot.title = element_markdown(family = "Open Sans", hjust = 0, size = 32, 
                                  face = 'bold', color = "#616466"),
    plot.subtitle = element_text(family = "Open Sans", hjust = 0, size = 18, 
                                   color = "#616466", margin = margin(b=20, t =10, l = -20)),
    
    plot.caption = element_markdown(family = "Open Sans", hjust = 1, size = 14, 
                                  color = "#616466", margin = margin(t=10)),
  )




#title and subtitle -----------------------------------------------------------

title = ggplot()+
  ggtitle(paste0(
      '<span style="color:',blue_google,'";>G</span>',
      '<span style="color:',red_google,'";>o</span>',
      '<span style="color:',yellow_google,'";>o</span>',
      '<span style="color:',blue_google,'";>g</span>',
      '<span style="color:',green_google,'";>l</span>',
      '<span style="color:',red_google,'";>e</span>',
      ' Trends 2020 en México.')) +
  theme(
    #title 
    plot.title = element_markdown(family = "Open Sans", hjust = 0, size = 32, 
                                  face = 'bold', color = "#616466", margin = margin(t = 10, b = 10))
  )

#Subtitle
subtitle = ggplot()+
  labs(subtitle = paste0('Recientemente Google publicó "El Año en Búsquedas para 2020". Descargué estos\n',
                         'datos para visualizar la evolución de cada tema en México. Los picos en la gráfica\n',
                         'representan los momentos en en que cada tema fue más popular.\n'
                         )
       ) +
  theme(
    #title 
    plot.subtitle = element_text(family = "Open Sans", hjust = 0, size = 18, 
                                 color = "#616466", margin = margin(b=20, t =10, l = -20))
  )



#grid plot, title, subtitle ---------------------------------------------------------------
final_plot = cowplot::plot_grid(title,subtitle, plot, rel_heights = c(.08,.15,1), ncol = 1)



#export
filename = file.path(dir_plots, "trends_MX.png")

ggsave(filename,
       plot = final_plot,
       height = 30,
       width = 25,
       units = 'cm'
       
       )
