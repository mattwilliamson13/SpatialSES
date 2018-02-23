##This code access the data files, cleans as necessary, and links to the FIPS codes for states and counties.
library(tidyverse)
library(tidycensus) #note: you will need a census api key, see ?tidycensus for details
library(tigris)

infolder <- "your data location"
 
#Set the area of interest
st <- c("CA","WA","OR")
st_full <- c("CALIFORNIA","WASHINGTON","OREGON")
census_api_key('YOUR_API_HERE')

##Social willingness variables
#Median Income
median_inc <- map_df(st, function(x) {
  get_acs(geography = "county", variables="B06011_001E", endyear = 2010, state=x, output = "wide")
})

#Education levels
education <- map_df(st, function(x) {
  get_acs(geography = "county", variables=c("B06009_001E", "B06009_005E", "B06009_006E"), endyear = 2010, state=x, output = "wide") %>%
    mutate(percDeg10 = ((B06009_005E + B06009_006E)/B06009_001E)*100)
})

#Read land values from NASS downloaded file
land_val <- read.csv(paste0(infolder,"AgLandValue_2012.csv"), colClasses = "character", stringsAsFactors = FALSE)

val_AOE <- land_val[land_val$State %in% st_full, ] #subset land value data for CA, OR, WA
val_AOE$State.ANSI <- stringr::str_pad(val_AOE$State.ANSI, 2, side="left",pad="0") #state ANSI codes should have 2 characters, leading 0's get stripped; add them back
val_AOE$County.ANSI <- stringr::str_pad(val_AOE$County.ANSI, 3, side="left", pad="0") #county ANSI codes should have 3 characters, leading 0's get stripped; add them back
val_AOE$GEOID <- paste0(val_AOE$State.ANSI, val_AOE$County.ANSI) #combine state and county ANSI codes to match GEOID for tidycensus data
val_AOE$Value <- gsub(",","",val_AOE$Value) #remove commas so that dollars is numeric

##Institutional capacity variables
##NonProfits
np_16 <- read.csv(paste0(infolder,"bmf.bm1608.csv"), stringsAsFactors = FALSE) #from the National Center for Charitable Statistics


