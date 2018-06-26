
# ==============================================================================
#                        2016
# ==============================================================================

# ---- Setup ----
library(ggplot2)
library(dplyr)
library(maptools)
library(scales)
#library(maps)
load("data/2016_sleepdef_clean.rdata")
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
          legend.text = element_text(size = 10),
          legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
          text = element_text(family = "Georgia", size = 12),
          axis.ticks.length = unit(0,"pt"),
          plot.margin = unit(c(0.1,0,0.1,0.1), "lines"))
us_map <- map_data("state")

# Continuous Hours Slept
cont <- ggplot() +
    geom_map(data=us_map, map=us_map,
             aes(long, lat, map_id=region),
             color="#b2b2b277", size=0.1, fill=NA) +
    geom_map(data=sleep_2016_cont, map=us_map,
             aes(fill=QLREST2.f, map_id=polyname),
             color="#b2b2b277", size=0.4) +
    ggtitle("Average Hours Slept") +
    scale_fill_distiller(name="Prevalence", palette="BrBG") +
    coord_map("polyconic") +
    scale_x_continuous(limits = c(-118,-75)) +
    scale_y_continuous(limits = c(25.5,49.38),expand = c(0,0)) +
    theme(axis.title = element_blank())+
    theme_map

ggsave(cont, file = "/home/michael/Code/Website/mgeden.github.io/images/sleepCDC/state_cont_plot.png",
       height=2.6, width = 4, units = "in", dpi = 400)

# DIchotomized Proportion Sleep Deprived
sleep_2016_dichot$QLREST2.f2 <- round(sleep_2016_dichot$QLREST2.f,digits =1)
dichot <- ggplot() +
    geom_map(data=us_map, map=us_map,
             aes(long, lat, map_id=region),
             color="#b2b2b277", size=0.1, fill=NA) +
    geom_map(data=sleep_2016_dichot, map=us_map,
             aes(fill=QLREST2.f2, map_id=polyname),
             color="#b2b2b277", size=0.4) +
    ggtitle("Proportion Sleep Deprived") +
    scale_fill_distiller(name="Prevalence", palette="BrBG",labels=percent) +
    coord_map("polyconic") +
    scale_x_continuous(limits = c(-118,-75)) +
    scale_y_continuous(limits = c(25.5,49.38),expand = c(0,0)) +
    #scale_fill_gradient(labels = percent) +
    theme(axis.title = element_blank())+
    theme_map

ggsave(dichot, file = "/home/michael/Code/Website/mgeden.github.io/images/sleepCDC/state_dichot_plot.png",
       height=2.6, width = 4, units = "in", dpi = 400)
