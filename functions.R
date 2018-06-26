
# ==============================================================================
# Extract Data from BRFSS and Pair with Coordinates
# ==============================================================================
# Only extracts 1 variable
# Latlong: If you want the latitude and longitude coordinates
# Reformat: Recode extraction variables BEFORE applying the function
# Function: Whatever you want to apply by the relevant grouping
BRFSS_geoextraction <- function(filepath, id = "county", position,
                                extraction, contiguous=TRUE,latlong=TRUE,
                                reformat = function(x) {x}, # Recode as NA for these values in extract
                                func = function(x) {mean(x, na.rm=T)}) {
    # Error Catching
    if(!grepl("state", tolower(position[1]))){stop("State must be first position.")}
    if(!is.character(filepath)) {stop("Filepath must be character")}
    if(!is.character(position)) {stop("Position must be character")}
    if(!is.character(extraction)) {stop("Extraction variable must be character name.")}
    if(length(extraction)!=1) {stop("Function currently only accepts one extracted var.")}
    if(!id %in% c("county", "state")) {stop("ID must be county or state.")}
    if(!is.function(func)){stop("Func must be a function.")}
    if(!is.function(reformat)) {stop("Reformat must be a function.")}
    if(id == "county" & length(position) != 2) {
        stop("County level requires position to have State and County varialbe names. ")
    }
    # Setup
    require(SASxport)
    require(dplyr)
    require(maps)
    require(stringr)
    require(ggplot2)
    strsplit2 <- function(x, char, index){
        unlist(strsplit(x, char))[index]
    }
    clear_labels <- function(x) {
        if(is.list(x)) {
            for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled')
            for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
        }
        else {
            class(x) <- setdiff(class(x), "labelled")
            attr(x, "label") <- NULL
        }
        return(x)
    }
    # Read Data
    data <- read.xport(filepath) %>%
        clear_labels(.)
    # Subset
    keep <- c(position, extraction)
    remove.states <- if(contiguous) {
        c(15, 2, 66, 72, 78)
    } else {c()}
    # Remove Excess
    data2 <- data %>%
        select(one_of(keep)) %>% # Select columns of interest
        filter(!(!!rlang::sym(position[1])) %in% remove.states) %>% # Select States
        filter(!(is.na(!!rlang::sym(position[1])))) # remove missing states
    if(id == "county"){
        data2 <- data2 %>%
            filter(!(!!rlang::sym(position[2])) %in% c(777,999)) %>%
            filter(!(is.na(!!rlang::sym(position[2])))) %>%
            mutate(FIPS = as.numeric(paste0(
                !!rlang::sym(position[1]), str_pad(!!rlang::sym(position[2]),3, pad = "0"), sep = "")
            ))
    } else {
        data2 <- data2 %>%
            mutate(FIPS = !!rlang::sym(position[1]))
    }
    # Get FIPS Codes
    if(id == "county") {
        data(county.fips)
        county.fips$polyname <- sapply(county.fips$polyname, function(x) {
            unlist(strsplit(x, ":"))[1]
        })
        county.fips <- unique(county.fips)
        fips.codes <- county.fips
    } else {
        data(state.fips)
        state.fips$polyname <- sapply(state.fips$polyname, function(x) {
            unlist(strsplit(x, ":"))[1]
        })
        state.fips <- unique(state.fips)
        fips.codes <- as.data.frame(state.fips)
    }
    varname <- paste0(extraction, ".f", sep = "")
    # Merge By FIPS
    data3 <- data2 %>%
        left_join(fips.codes, by = c("FIPS" = "fips")) %>% # Get location string
        mutate(temp = reformat(!!rlang::sym(extraction))) %>%
        group_by(polyname,FIPS) %>% # State/County-wise operations
        summarize(!!varname := func(temp), # Proportion of sleep deficiency
                  n = sum(!is.na(temp)), # Frequency of response
                  prop.responded = sum(!is.na(temp))/n()) %>% # Proportion of Responses
        filter(!is.na(!!rlang::sym(varname))) %>% # Remove Missing
        mutate(state = strsplit2(polyname, ",", 1)) # Extract state name
    if(id == "county") {
        data3$county <- strsplit2(data3$polyname, ",",2)
    }
    # Merge for coordinates
    if(id == "county"){
        map <- map_data('county') %>%
            mutate(polyname = paste0(region, ",", subregion, sep = "")) %>%
            select(-c(group, order, region, subregion))
    } else {
        map <- map_data('state') %>%
            mutate(polyname = region) %>%
            select(-c(group, order, region, subregion))
    }
    if(latlong) {data3 <- left_join(data3, map, by = "polyname")}
    as.data.frame(data3)
}

# ================================== PLOT =====================================
library(ggplot2)
theme_map <- theme_bw() +
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = c(.95, .2),
          #legend.title = element_text(size = 14, face="bold"),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          plot.title = element_text(size = 10))
# Functinotitl
geoplot_cont <- function(dat, fill, distiller.palette = "Spectral",level="county") {
    if(!level %in% c("state", "county")) {stop("Level must be state or COunty.")}
    state.df <- map_data("state")
    if(level == "county"){
    ggplot(dat, aes_string(x="long", y="lat", group="polyname", fill=fill)) +
        geom_polygon(color = "black", size = .15)+
        geom_polygon(data = state.df, aes(x=long, y = lat, group = group, fill = NA),
                     color = "black", size = .8, fill = NA)+
        scale_fill_distiller(palette = distiller.palette) +
        coord_map("albers", lat0=39, lat1=45,
                  xlim = c(-120, -73),
                  ylim = c(25.12993,49.38323))
    } else {
        state.df$polyname <- state.df$region
        ggplot(dat, aes(map_id = polyname)) +
            geom_map(aes_string(fill=fill), map = state.df, color = "black", size = .2) +
            expand_limits(x = state.df$long, y = state.df$lat)+
            scale_fill_distiller(palette = distiller.palette) +
            coord_map("albers", lat0=39, lat1=45,
                      xlim = c(min(state.df$long), max(state.df$long)),
                      ylim = c(min(state.df$lat), max(state.df$lat)))
    }
}
geoplot_n <- function(dat, fill, min = 30) {
    n <- pull(dat, fill)
    dat$n.binom <- ifelse(n<min, 1, 0)
    state.df <- map_data("state")
    ggplot(dat, aes(x=long, y=lat, group=polyname, fill=n.binom)) +
        geom_polygon(data = state.df, aes(x=long, y = lat, group = group),
                     color = NA, size = .6, fill = "grey") +
        geom_polygon(data = dat, aes(x=long, y=lat, group=polyname, fill=n.binom),
                     color = "black", size = .1, linetype = 4)+
        geom_polygon(data = state.df, aes(x=long, y = lat, group = group, fill = NA),
                     color = "black", size = .6, fill = NA)+
        scale_fill_continuous(low = "white", high = "red")+
        coord_map("albers", lat0=39, lat1=45,
                  xlim = c(-120, -73),
                  ylim = c(25.12993,49.38323)) +
        theme_map +
        theme(legend.position = "none")
}
geoplot_quintiles <- function(dat, fill, brewer.palette = "YlOrRd", level = "county") {
    state.df <- map_data("state")
    quintiles <- function(x){
        qnt <- quantile(x, seq(0,1, by=.20))
        cut <- cut(x, unique(qnt), include.lowest=TRUE,
                   ordered_result = TRUE)
        cut
    }
    dat$discrete <- quintiles(pull(dat, fill))
    if(level == "county"){
    ggplot(dat, aes(x=long, y=lat, group=polyname, fill=discrete)) +
        geom_polygon(color = "black", size = .15)+
        geom_polygon(data = state.df, aes(x=long, y = lat, group = group, fill = NA),
                     color = "black", size = .8, fill = NA)+
        coord_map("albers", lat0=39, lat1=45,
                  xlim = c(-120, -73),
                  ylim = c(25.12993,49.38323)) +
        scale_fill_brewer(palette= brewer.palette)
    } else {
    state.df$polyname <- state.df$region
    ggplot(dat, aes(map_id = polyname)) +
        geom_map(aes(fill=discrete), map = state.df, color = "black", size = .2) +
        expand_limits(x = state.df$long, y = state.df$lat)+
        scale_fill_brewer(palette= brewer.palette)+
        coord_map("albers", lat0=39, lat1=45,
                  xlim = c(min(state.df$long), max(state.df$long)),
                  ylim = c(min(state.df$lat), max(state.df$lat)))
    }
}
save(file="data/functions.rdata", BRFSS_geoextraction, geoplot_cont,
     geoplot_n,geoplot_quintiles, theme_map)
