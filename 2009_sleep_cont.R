
library(ggplot2)
library(dplyr)
library(maps)
library(stringr)
load("data/2009_sleepdef_clean.rdata")
# Theme
theme_map <- theme_bw() +
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = c(.95, .2),
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          text = element_text(family = "Georgia", size = 12),
          axis.title = element_blank(),
          axis.ticks.length = unit(0,"pt"),
          plot.margin = unit(c(0.1,0,0.1,0.1), "lines"))

ggplot(sleep_2009, aes(x=long, y=lat, group=polyname, fill=prop)) +
    geom_polygon(color = "grey44", size = .4)+
    geom_polygon(data = state.df, aes(x=long, y = lat, group = group, fill = NA),
                 color = "black", size = .6, fill = NA)+
    scale_fill_distiller(palette = "Spectral") +
    coord_map("albers", lat0=39, lat1=45,
              xlim = c(-120, -73),
              ylim = c(25.12993,49.38323)) +
    ggtitle("Continuous Proportion of Sleep Deprivation") +
    theme_map
ggsave(file = "/home/michael/Code/Website/mgeden.github.io/images/sleepCDC/county_cont_plot.png",
       height=5.2, width = 8, units = "in", dpi = 400)

