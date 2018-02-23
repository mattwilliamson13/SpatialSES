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
#NonProfits
np_16 <- read.csv(paste0(infolder,"bmf.bm1608.csv"), stringsAsFactors = FALSE) #from the National Center for Charitable Statistics
np_16$FIPS <- ifelse(nchar(np_16$FIPS) == 4, paste0("0", np_16$FIPS), np_16$FIPS) #add leading zeros
np_16aoe <- np_16[np_16$STATE == "CA" | np_16$STATE == "OR" | np_16$STATE == "WA",] #subset to Area of Interest
np_16_sum <- np_16aoe %>% group_by(FIPS, LEVEL4) %>% summarise(numOrg16 = n()) #Group non-profits by code denoting NAICS type
np_16_tot <- np_16_sum %>% group_by(FIPS) %>% summarise(totNP16 = n()) #sum across all organization types for each county
colnames(np_16_tot)[1] <- "GEOID"

#County Governments
co_emp_dat_12 <- read.table(paste0(infolder,"12coar2.dat", colClasses="character") #load 2012 county employment data; fields described in  County Area Data File Record Layout.pdf
colnames(co_emp_dat_12) <- c("stcode","ctycode","dataID","FTE","FTP","PTE","PTP","PTH","FTEE","TOTEMP","TOTPAY")
co_emp_inf_12 <- read.fwf(paste0(infolder,"12coar1.dat"), widths=c(2,1,3,8,35,29,1,30,2,3,5,4,2,9,2,61,2),colClasses='character',strip.white=TRUE) #load county area ID file described in County Area ID File Record Layout.pdf
co_emp_fips_12 <- co_emp_inf_12[,c(1,3,8:10),] #get fips codes
colnames(co_emp_fips_12) <- c("stcode","ctycode", "ctyname","sfp","cfp")
co_emp_dat_12_merge <- merge(co_emp_dat_12, co_emp_fips_12, by=c("stcode","ctycode"),all.x=TRUE) #merge fips data with employment data
AOE_Emp <- co_emp_dat_12_merge[co_emp_dat_12_merge$sfp == "06" | co_emp_dat_12_merge$sfp == "53" | co_emp_dat_12_merge$sfp == "41",  ] #CA, OR, and WA FIPS Codes are 06, 53, 41
AOE_totEmp <- AOE_Emp[AOE_Emp$dataID == "000",] #select the "total employee" record
AOE_totEmp$GEOID <- paste0(AOE_totEmp$sfp, AOE_totEmp$cfp) # create 5 digit geoid for merging with tidycensus data

#Rural Development Grants
RD_grants <- read.csv(paste0(infolder,"USDAComFac_GrantsOnly.csv"),colClasses = "character", stringsAsFactors = FALSE) #from data.gov
colnames(RD_grants)[1:2] <- c("RD_st","RD_cty") #USDA uses obscure codes for state and county (non-ANSI)
RD_sfp <- read.csv(paste0(infolder,"RD_sfp_xwalk.csv"), colClasses = "character") #crosswalk sfp and cfp to RD state and county codes per Gayle Doss
colnames(RD_sfp)[1:2] <- c("RD_st","RD_cty")
RD_join <- RD_grants %>% left_join(RD_sfp) #join ANSI codes to Grants Data
RD_grant_sub <- RD_join[,c(31:34,5,7,14)] #retain only ID info and grant amount
RD_grant_sub$Obligation.Amount <- gsub(",","", RD_grant_sub$Obligation.Amount) #necessary to conver to numeric
RD_grant_sub$Obligation.Amount <- gsub(" \\$","", RD_grant_sub$Obligation.Amount) #necessary to conver to numeric
RD_grant_sub$Obligation.Amount <- as.numeric(RD_grant_sub$Obligation.Amount) #to allow summation across all grants in the county
RD_grant_sum <- RD_grant_sub %>% group_by(St.Cd.Fips, Cty.Cd.Fips) %>%
                  summarise(totGrant = sum(Obligation.Amount)) #sum of grants made to county
RD_AOE <- RD_grant_sum[RD_grant_sum$St.Cd.Fips == "06" | RD_grant_sum$St.Cd.Fips == "53" | RD_grant_sum$St.Cd.Fips == "41",] #subset to AOE
RD_AOE$GEOID <- paste0(RD_AOE$St.Cd.Fips, RD_AOE$Cty.Cd.Fips) #create GEOID lookup


