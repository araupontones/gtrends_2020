#set up
library(pacman)
p_load(gtrendsR, lubridate, extrafont, tidyverse, ggtext, cowplot)


loadfonts(dev = 'win')
Sys.setlocale("LC_TIME", "Spanish")

dir_plots = 'plots'
