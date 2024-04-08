## CUSTOM THEME ####

library(ggthemes)
library(extrafont)
library(tidyverse)

theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='avenir'), 
      axis.title.x = element_text(face='italic', size=12, margin=margin(t=10)),
      axis.title.y = element_text(face='italic', size=12, margin=margin(r=10), angle = 90),
      axis.text.x = element_text(face='bold.italic'),
      axis.text.y = element_text(face='bold.italic'), 
      panel.background = element_rect('floralwhite'), 
      plot.background = element_rect('floralwhite'),
      plot.title = element_text(face='bold', size=17, hjust = 0.5),
      plot.subtitle=element_text(face='italic', size=12, hjust = 0.5, vjust = -1), 
      panel.grid.major = element_line(color='gray90', linetype = 'dashed'),
      plot.margin = margin(.5, .5, .25, .5, "cm")
      ) 
}


  
