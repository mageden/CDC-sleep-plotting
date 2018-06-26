
# ==============================================================================
#             Frequency of Response and Proportion Responded
# ==============================================================================

# ---- Setup ----
library(ggplot2)
library(dplyr)
library(maptools)
library(scales)
#library(maps)
load("data/sleep_2016.rdata")
# Theme
theme_map <- theme_bw() +
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = c(.96, .2),
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
          text = element_text(family = "Georgia", size = 12),
          axis.ticks.length = unit(0,"pt"),
          plot.margin = unit(c(0.1,0,0.1,0.1), "lines"))
us_map <- map_data("state")

# Frequency
freq <- ggplot() +
    geom_map(data=us_map, map=us_map,
             aes(long, lat, map_id=region),
             color="#b2b2b277", size=0.1, fill=NA) +
    geom_map(data=sleep_2016_cont, map=us_map,
             aes(fill=n, map_id=polyname),
             color="#b2b2b277", size=0.4) +
    ggtitle("Sample Size") +
    scale_fill_distiller(name="Prevalence", palette="BrBG",
                         breaks=c(10000,20000,30000),
                         labels=c("10k","20k","30k")) +
    coord_map("polyconic") +
    scale_x_continuous(limits = c(-118,-75)) +
    scale_y_continuous(limits = c(25.5,49.38),expand = c(0,0)) +
    theme(axis.title = element_blank())+
    theme_map
ggsave(freq, file = "/home/michael/Code/Website/mgeden.github.io/images/sleepCDC/state_freq_plot.png",
       height=2.6, width = 4, units = "in", dpi = 400)

# Proportion Missing
prop.resp <- ggplot() +
    geom_map(data=us_map, map=us_map,
             aes(long, lat, map_id=region),
             color="#b2b2b277", size=0.1, fill=NA) +
    geom_map(data=sleep_2016_cont, map=us_map,
             aes(fill=prop.responded, map_id=polyname),
             color="#b2b2b277", size=0.4) +
    ggtitle("Proportion Responded") +
    scale_fill_distiller(name="Prevalence", palette="BrBG", label = percent) +
    coord_map("polyconic") +
    scale_x_continuous(limits = c(-118,-75)) +
    scale_y_continuous(limits = c(25.5,49.38),expand = c(0,0)) +
    theme(axis.title = element_blank())+
    theme_map
ggsave(prop.resp, file = "/home/michael/Code/Website/mgeden.github.io/images/sleepCDC/state_resp_plot.png",
       height=2.6, width = 4, units = "in", dpi = 400)
