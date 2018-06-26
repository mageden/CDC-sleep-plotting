
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
          legend.position = c(.94, .2),
          legend.title = element_blank(),
          legend.text = element_text(size = 8))
quintiles <- function(x){
    qnt <- quantile(x, seq(0,1, by=.20))
    cut <- cut(x, unique(qnt), include.lowest=TRUE,
               ordered_result = TRUE)
    lvls <- levels(cut)
    newlevels <- sapply(lvls, function(x) {
        x2 <- substr(x, 2, nchar(x)-1)
        x3 <- gsub(",","-",x2)
        x3
    })
    output <- plyr::mapvalues(cut, from = lvls, to = newlevels)
    output
}

sleep_2009$discrete <- quintiles(sleep_2009$prop)
ggplot(sleep_2009, aes(x=long, y=lat, group=polyname, fill=discrete)) +
    geom_polygon(color = "gray55", size = .4)+
    geom_polygon(data = state.df, aes(x=long, y = lat, group = group, fill = NA),
                 color = "black", size = .6, fill = NA)+
    coord_map("albers", lat0=39, lat1=45,
              xlim = c(-120, -73),
              ylim = c(25.12993,49.38323)) +
    scale_fill_brewer(palette="YlOrRd")+
    ggtitle("Quintiles of Proportion of Sleep Deprivation") +
    theme_map +
    theme(text = element_text(family = "Georgia", size = 12),
          axis.title = element_blank(),
          axis.ticks.length = unit(0,"pt"),
          plot.margin = unit(c(0.1,0,0.1,0.1), "lines"))
ggsave(file = "/home/michael/Code/Website/mgeden.github.io/images/sleepCDC/county_quint_plot.png",
       height=5.2, width = 8, units = "in", dpi = 400)
