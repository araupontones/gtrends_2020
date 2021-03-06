source("set_up.R")
source("styles.R")
source("functions.R")




#terms to search
terms = c(
  
  
  "que es tusa",
  "que es el coronavirus",
  "que es una pandemia",
  "que es un verso",
  "que paso el dia que nac�",
  "que es el estr�s",
  "que son los alimentos procesados",
  "que es la lepra",
  "que es un shock hipovol�mico",
  
  
  "que es el c�ncer de mama"
  
 
  
  
  
 
  
  
  
)

terms_suspensivos = paste0(str_replace(terms,"que ", "..." ), "?")




#download data from gtrends
data_google_list = map(terms,get_trend, geo="MX")


data_google = do.call(rbind, data_google_list) %>%
  mutate(term = str_replace(term, "que ", "..."),
         term = paste0(term, "?"),
         term = factor(term,
                       levels  = terms_suspensivos,
                       ordered = T
         ))



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
    labels = function(x){str_to_title(lubridate::month(x, label = T, abbr = F))}
  ) +
  
  #'title and subtitle ---------------------------------------------------------
  labs(
    title = paste0(
      '<span style="color:',blue_google,'";>G</span>',
      '<span style="color:',red_google,'";>o</span>',
      '<span style="color:',yellow_google,'";>o</span>',
      '<span style="color:',blue_google,'";>g</span>',
      '<span style="color:',green_google,'";>l</span>',
      '<span style="color:',red_google,'";>e</span>',
      ' Trends 2020 en M�xico.<br>',
      '<span align="center";>Top 10 �Qu�?</span>'),
    
    subtitle = paste0('La gr�fica muestra el top 10 de las b�squedas relacionadas con �Qu�?...\n',
                      'y el per�odo en que su b�squeda fue m�s frecuente durante 2020 en\n',
                      'M�xico. La b�squeda m�s popular fue �Qu� es el coronavirus?\n',
                      'la cual alcanz� su m�xima popularidad a finales de enero.'),
    
    caption = "**Data:** trends.google.com, diciembre 2020 | **Chart:** Andres Arau | **Inspired by:** Roshaan Khan"
    
  ) +
  
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
    strip.text.y.left= element_text(angle=360, family = "Open Sans", hjust = 0, size = 14, 
                                    face = 'bold', color = "#616466"),
    strip.background = element_blank(),
    
    #'legend
    legend.position = 'none',
    
    #title 
    plot.title = element_markdown(family = "Open Sans", hjust = .5, size = 32, 
                                  face = 'bold', color = "#616466"),
    
    plot.subtitle = element_text(family = "Open Sans", hjust = 0, size = 18, 
                                 color = "#616466", margin = margin(b=20, t =10)),
    
    plot.caption = element_markdown(family = "Open Sans", hjust = 1, size = 14, 
                                    color = "#616466", margin = margin(t=10)),
  )




filename = file.path(dir_plots, "que_MX.png")

ggsave(filename,
       plot = plot,
       height = 25,
       width = 33,
       units = 'cm'
       
)
