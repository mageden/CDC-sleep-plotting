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
          legend.text = element_text(size = 8))

# State data used for state boundaries
state.df <- map_data("state")
sleep_2009$n.binom <- ifelse(sleep_2009$n<31, 1, 0)
ggplot(sleep_2009, aes(x=long, y=lat, group=polyname, fill=n.binom)) +
    geom_polygon(data = state.df, aes(x=long, y = lat, group = group),
                 color = NA, size = .6, fill = "firebrick") + # Make background grey
    geom_polygon(data = sleep_2009, aes(x=long, y=lat, group=polyname, fill=n.binom),
                 color = "grey27", size = .3)+ # Fill in red
    geom_polygon(data = state.df, aes(x=long, y = lat, group = group, fill = NA),
                 color = "black", size = .6, fill = NA)+ # Put in state lines
    scale_fill_continuous(low = "white", high = "indianred")+
    scale_x_continuous(expand = c(0,0)) +
    coord_map("albers", lat0=39, lat1=45,
              xlim = c(-120, -73),
              ylim = c(25.12993,49.38323)) +
    labs(title = "Frequency of Responses",
         subtitle = "Dark red are regions with no data, and light red n<31") +
    theme_map +
    theme(legend.position = "none",
          text = element_text(family = "Georgia", size = 12),
          axis.title = element_blank(),
          axis.ticks.length = unit(0,"pt"),
          plot.margin = unit(c(0.1,0,0.1,0.1), "lines"))
ggsave(file = "/home/michael/Code/Website/mgeden.github.io/images/sleepCDC/county_miss_plot.png",
       height=5.2, width = 8, units = "in", dpi = 400)
