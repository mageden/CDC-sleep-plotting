
# ==============================================================================
#               Sleep Quantity over Time by County
# NOTES
    # Check that all years have the same names for STATE, COUNTY, and QLREST2, and
    # the same codes for missing data
        # QLREST2: 2011-2012
        # SLEPTIME: 2011-2014,2016; Name changes from 2011-2012 | 2013-2016
# Function Outline
    # 1. Loop Through list of Dataframes
        # Remove rows with missing primary varaible or id variable
    # 2. Perform summarization on a variable by an ID variable
    # 3. Aggregate lists
# Post-processing
    # Recode missings
    # Associate with LAT and LONG
# ==============================================================================
library(SASxport)
library(ggplot2)
library(dplyr)
library(maps)
library(stringr)

# Setup inputs
raw.files <- grep("LLCP",paste0("data/raw/", list.files("data/raw/"), sep = ""), value = TRUE)
raw.files <- raw.files[c(1:4,6)] # These are where sleptime are saved
id <- rep("X.STATE", length(raw.files))
extraction <- c("SLEPTIME", "SLEPTIME", "SLEPTIM1", "SLEPTIM1", "SLEPTIM1")
time <- sapply(raw.files, function(x){
    as.numeric(str_extract_all(x, "[0-9]+")[[1]])
})
# ---- TESTING ZONE -----
filepaths = raw.files
f = filepaths[1]
i=1
# ----------------------
# SLEPTIME NA ARE HARDCODED IN
# Function for consolidating data
consolidate_time_series <- function(filepaths, id, extraction, time) {
    require(dplyr)
    # Error Checking
    if(!is.vector(filepaths)|!is.character(filepaths)){
        stop("Filepaths must be a character vector.")
    }
    if(!is.vector(extraction)|!is.character(extraction)){
        stop("Extraction must be a character vector.")
    }
    if(!is.vector(time)|!is.numeric(time)){
        stop("Time must be a numeric vector.")
    }
    if(!is.vector(id)|!is.character(id)){
        stop("ID must be a character vector")
    }
    if(!isTRUE(all.equal(length(filepaths), length(extraction), length(time), length(id)))){
        stop("Filepaths, extraction, and time must be the same length.")
    }
    # Setup: Extract Filename for naming purposes
    filenames <- sapply(filepaths, function(x) {
        temp1 <- unlist(strsplit(x[[1]], "/"))
        temp1 <- temp1[length(temp1)]
        unlist(strsplit(temp1, "[.]"))[1]
    })
    # Create Bucket
    data.list <- rep(list(list()), length(filepaths))
    names(data.list) <- filenames
    # Loop Through Files
    for(i in 1:length(filepaths)){
        print(i)
        temp.extraction <- extraction[i]
        temp.id <- id[i]
        temp.time <- time[i]
        rawdat <- read.xport(filepaths[i])
        data <- rawdat %>%
            select(one_of(c(temp.id, temp.extraction))) %>%
            mutate(tmep = replace(!!r:lang::sym(temp.extraction), !!r:lang::sym(temp.extraction) > 24, NA)) %>%
            group_by(!!rlang::sym(temp.id)) %>%
            summarise(n.total = n(),
                      n.resp = sum(!is.na(!!rlang::sym(temp.extraction))),
                      prop.miss = sum(!is.na(!!rlang::sym(temp.extraction)))/n(),
                      mean = mean(!!rlang::sym(temp.extraction), na.rm = TRUE)) %>%
            mutate(time = temp.time)
        data.list[[i]] = data
    }
    data.list
}
test <- consolidate_time_series(filepaths = raw.files,
                                extraction = extraction,
                                id = id,
                                time = time)
data(state.fips)
state.fips$polyname <- sapply(state.fips$polyname, function(x) {
    unlist(strsplit(x, ":"))[1]
})
state.fips <- unique(state.fips)
test2 <- bind_rows(test) %>%
    filter(!X.STATE %in% c(66, 72, 78)) %>% # remove Guam, PR, and VI
    filter(!is.na(X.STATE)) %>%
    mutate(FIPS = X.STATE) %>%
    left_join(state.fips, by = c("FIPS" = "fips")) # Get location string
