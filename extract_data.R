
# ================== Clean Data ==============
# ------------ 2009 -----------
# 2009 County Data
load("data/functions.rdata")
filepath <- "data/raw/CDBRFS09.XPT"
position <- c("X.STATE", "CTYCODE")
extraction <- c("QLREST2")
QLREST2_format <- function(x) {
    x[x==88] <- 0
    x[x>30] <- NA
    x <- ifelse(x>14, 1, 0)
    x
}
sleep_2009_county <- BRFSS_geoextraction(filepath = filepath,
                            position = position,
                            extraction = extraction,
                            id = "county",
                            reformat = QLREST2_format)

# 2009 State Data
position <- c("X.STATE")
sleep_2009_state <- BRFSS_geoextraction(filepath = filepath,
                                         position = position,
                                         extraction = extraction,
                                         id = "state",
                                         reformat = QLREST2_format,
                                         latlong = FALSE)

# ---- Save data ----
save(file = "data/sleep_2009.rdata", sleep_2009_state, sleep_2009_county)

# ------------ 2016 ------------
# 2016 County Data
load("data/functions.rdata")
filepath <- "data/raw/LLCP2016.XPT"
position <- "X.STATE"
extraction <- c("SLEPTIM1")
SLEPTIM_format <- function(x) {
    x[x>24] <- NA
    x
}

# 2016 Continuous
position <- c("X.STATE")
sleep_2016_cont <- BRFSS_geoextraction(filepath = filepath,
                                        position = position,
                                        extraction = extraction,
                                        id = "state",
                                        reformat = SLEPTIM_format,
                                        latlong = FALSE)
# Dichotimized
SLEPTIM_format_dichot <- function(x) {
    x[x>24] <- NA
    x <- ifelse(x<7, 1, 0)
    x
}
sleep_2016_dichot <- BRFSS_geoextraction(filepath = filepath,
                                       position = position,
                                       extraction = extraction,
                                       id = "state",
                                       reformat = SLEPTIM_format_dichot,
                                       latlong = FALSE)
# ---- Save data ----
save(file = "data/sleep_2016.rdata", sleep_2016_cont, sleep_2016_dichot)
