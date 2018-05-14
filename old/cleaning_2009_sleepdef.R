
# ============================================================================
#                    Cleaning 2009 CDC Data
# Sleep Related Variables
    # USING:
        # QLREST2:
    # NOT USING: Mostly missing data, need to aggregate across years to use
        # SLEPTIME:
        # SLEPDAY:
        # SLEPDRIV:
# NOTES
    # There are a few FIPS with multiple counties; these are part of the merging
        # county.fips dataset, though I am not sure how appropriate this is.
# ISSUES
    # The final data has proportions that are different thant he one in the paper by a large
        # degree, with many responses above what they report as their max.
# ============================================================================
# ------------ Setup ------------
library(SASxport)
library(ggplot2)
library(dplyr)
library(maps)
library(stringr)
# ---- Functions ----

# -------- ---- Cleaning up ------------
# ---- Read in Data ---
data <- read.xport("data/raw/CDBRFS09.XPT")

# ---- Select Columns of interest ----
# Position (State FIPS code + County code)
position <- c("X.STATE","CTYCODE")
# Sleep Related
sleep <- c("QLREST2")
# Keep
keep <- c(position, sleep)
# ---- Remove Excess ----
data2 <- data %>%
    select(one_of(keep)) %>% # Select columns of interest
    filter(!X.STATE %in% c(15, 2, 66, 72, 78)) %>% # remove Guam, PR, and VI
    filter(!CTYCODE %in% c(777,999)) %>% # Remove responses not reporting county
    filter(!is.na(CTYCODE)) %>% # Remove missing counties
    filter(!is.na(X.STATE)) %>% # remove missing states
    mutate(FIPS = as.numeric(paste0(X.STATE, str_pad(CTYCODE,3, pad = "0"), sep = "")))

# ---- Extract Spatial Information from Codes ----
data(county.fips) # Numeric FIPS paired with state and county names
county.fips$polyname <- sapply(county.fips$polyname, function(x) {
    unlist(strsplit(x, ":"))[1]
})
county.fips <- unique(county.fips)

# ---- One-to-Many Merge County Fips ----
# Create Function
strsplit2 <- function(x, char, index){
    unlist(strsplit(x, char))[index]
}
# Merge with FIPS, and then summarize information by county
# ===================
# Our data =   6368 missing responses for insufficient sleep;
#     total = 432607
#     counties = 2210
# Their data = 7619 missing responses for insufficient sleep;
#     total = 424989
#     counties = 2231
# ===================
data3 <- data2 %>%
    left_join(county.fips, by = c("FIPS" = "fips")) %>% # Get location string
    mutate(QLREST2 = replace(QLREST2, QLREST2 == 88, 0)) %>%
    mutate(QLREST2 = replace(QLREST2, QLREST2 > 30, NA)) %>% # Recode didn;t know
    mutate(dQLREST = ifelse(QLREST2>14,1,0)) %>%  # Dichotomize (based on Grandner et al., 2016))
    #filter(!is.na(QLREST2)) %>% # Remove missing
    group_by(polyname,FIPS) %>% # County-wise operations
    summarize(prop = mean(dQLREST, na.rm = TRUE), # Proportion of sleep deficiency
              n = sum(!is.na(dQLREST)), # Frequency of response
              prop.responded = sum(!is.na(dQLREST))/n()) %>% # Proportion of Responses
    filter(!is.na(prop)) %>% # Remove Missing Counties
    mutate(state = strsplit2(polyname, ",", 1), # Extract state name
           county = strsplit2(polyname, ",",2)) %>% # Extract county name
    ungroup()

# Merge:
map.county <- map_data('county') %>%
    mutate(polyname = paste0(region, ",", subregion, sep = "")) %>%
    select(-c(group, order, region, subregion))
data4 <- left_join(data3, map.county, by = "polyname")
#data4$polyname <- NULL

sleep_2009 <- data4
# ------------------------ SAVE -------------------------
save(file = "data/2009_sleepdef_clean.rdata", sleep_2009)

# ================================== PLOT =====================================

# Theme
theme_map <- theme_bw() +
    theme(axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = c(.95, .2),
          legend.title = element_text(size = 14, face="bold"),
          legend.text = element_text(size = 12))
# Plotting data
state.df <- map_data("state")
# Function
geoplot_cont <- function(dat, fill, distiller.palette = "Spectral") {
    state.df <- map_data("state")
    theme_map <- theme_bw() +
        theme(axis.text = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              panel.border = element_blank(),
              panel.grid = element_blank(),
              axis.title = element_blank(),
              legend.position = c(.95, .2),
              legend.title = element_text(size = 14, face="bold"),
              legend.text = element_text(size = 12))
    ggplot(dat, aes_string(x="long", y="lat", group="polyname", fill=fill)) +
        geom_polygon(color = "black", size = .15)+
        geom_polygon(data = state.df, aes(x=long, y = lat, group = group, fill = NA),
                     color = "black", size = .8, fill = NA)+
        scale_fill_distiller(palette = distiller.palette) +
        coord_map("albers", lat0=39, lat1=45,
                  xlim = c(-120, -73),
                  ylim = c(25.12993,49.38323)) +
        theme_map
}
geoplot_quintiles <- function(dat, fill, brewer.palette = "YlOrRd") {
    state.df <- map_data("state")
    theme_map <- theme_bw() +
        theme(axis.text = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              panel.border = element_blank(),
              panel.grid = element_blank(),
              axis.title = element_blank(),
              legend.position = c(.95, .2),
              legend.title = element_text(size = 14, face="bold"),
              legend.text = element_text(size = 12))
    quintiles <- function(x){
        qnt <- quantile(x, seq(0,1, by=.20))
        cut <- cut(x, unique(qnt), include.lowest=TRUE,
                   ordered_result = TRUE)
        cut
    }
    dat$discrete <- quintiles(pull(dat, fill))
    ggplot(dat, aes_string(x="long", y="lat", group="polyname", fill="discrete")) +
        geom_polygon(color = "black", size = .15)+
        geom_polygon(data = state.df, aes(x=long, y = lat, group = group, fill = NA),
                     color = "black", size = .8, fill = NA)+
        coord_map("albers", lat0=39, lat1=45,
                  xlim = c(-120, -73),
                  ylim = c(25.12993,49.38323)) +
        scale_fill_brewer(palette= brewer.palette)+
        theme_map
}
# ---------------------- STATE --------------------------

# ---- Plot of Response Frequency ---
geoplot_cont(sleep_2009_county, "n")+theme_map
# ---- Plot of Response Proportion ---
geoplot_cont(sleep_2009_county, "prop.responded")+theme_map
# ---- Plot of Continuous Proportion ----
geoplot_cont(sleep_2009_county, "QLREST2.f")+theme_map
# ---- Plot of discrete percentages ----
geoplot_quintiles(sleep_2009_county, "QLREST2.f")+theme_map
