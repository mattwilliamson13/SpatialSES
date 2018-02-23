##This code access the data files, cleans as necessary, and links to the FIPS codes for states and counties.
library(tidyverse)
library(tidycensus) #note: you will need a census api key, see ?tidycensus for details
library(tigris)

infolder <- "your data location"
 
#Set the area of interest
st <- c("CA","WA","OR")
st_full <- c("CALIFORNIA","WASHINGTON","OREGON")
census_api_key('YOUR_API_HERE')

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

