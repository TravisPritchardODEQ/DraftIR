library(tidyverse)
library(readxl)
library(stringr)
require(rgdal)
require(RODBC)

#disable scientific notation
options(scipen = 999)



# Load data and combine ---------------------------------------------------

# Load criteria data from ambient program
load("Data Validation/anom_crit.Rdata")


# Load in template projects sheet 
projects_import <- read_excel("Data Validation/Test_Data_For_Submission.xlsx", sheet = "Projects")
# Load in template monitoring locations
monitoring_locations_import <- read_excel("Data Validation/Test_Data_For_Submission.xlsx", sheet = "Monitoring Locations")
#load in template results
Results_import <-  read_excel("Data Validation/Test_Data_For_Submission.xlsx", sheet = "Results")

#rename columns to fit R style better
##### Fix this to be more consistant
colnames(Results_import) <- make.names(names(Results_import), unique=TRUE)
colnames(monitoring_locations_import) <- make.names(names(monitoring_locations_import), unique=TRUE)

#filter out empty rows from the import and combine results with monitoring locations
#filter out data that is not surface water data
Results <- Results_import %>%
  filter(!is.na(Result.Value)) %>%
  left_join(monitoring_locations_import, by = "Monitoring.Location.ID" ) %>%
  filter(Activity.Media.Subdivision.Name == "Surface Water")
  


# assign regions based on info from stations table -------------------------


#get station table

#connect to view as a general user 
sta.sql = odbcConnect('Stations')

#pull in stations table
Stations = sqlFetch(sta.sql, "VWStationsFinal") 

#reformat some fields to char to avoid sci notation
# Stations <- Stations %>%
#   mutate(Reachcode = as.character(Stations$Reachcode))

odbcClose(sta.sql)

#merge Results table with stations
HUC4 <- Stations %>%
  select(STATION_KEY, HUC4_Name) %>%
  mutate(Monitoring.Location.ID = paste0(STATION_KEY, "-ORDEQ")) %>%
  select(-STATION_KEY)

Results <- Results %>%
  left_join(HUC4, by = "Monitoring.Location.ID") %>%
  mutate(HUC4_Name = as.character(HUC4_Name))



# comparison --------------------------------------------------------------

#read in file to convert template characteritic name to internal version
#this isn't finished
char_converter <- read.csv("Data Validation/char_converter.csv", na.strings=c("","NA"), stringsAsFactors = FALSE )

#deal with censored data
#lines 67 - 70 is where we would replace censored data once that has been finalized
Results_censored <- Results %>%
  mutate(r_qual = ifelse(!is.na(as.numeric(Result.Value)), "=", 
                    ifelse(str_sub(Result.Value, 1, 1) == "<", "<",
                           ifelse(str_sub(Result.Value, 1, 1) == ">", ">",
                                  ifelse(str_sub(Result.Value, -3, -1) == "Est", "Est", "other"))))) %>%
  mutate(r = ifelse(r_qual == "=", as.numeric(Result.Value),
                    ifelse(r_qual == ">", as.numeric(str_sub(Result.Value, 2, nchar(Result.Value))),
                           ifelse(r_qual == "<", Detection.Limit.Value..2,
                                  ifelse(r_qual == "Est", as.numeric(str_sub(Result.Value, 1, 3)), "other"))))) 



#compare results to 1st and 99th percentiles
Results_validated <- Results_censored %>%
  left_join(char_converter, by = "Characteristic.Name" ) %>%
  left_join(anom_crit, by = c("ORDEQ_char", "HUC4_Name" = "StationHuc4")) %>%
  mutate(r = as.numeric(r)) %>%
  mutate(validate_flag = ifelse(r < per1, "Below 1%",
                                ifelse(r > per99, "Above 99%", "Acceptable"))) 


Validated_stats <- Results_validated %>%
  group_by(validate_flag) %>%
  summarise(num = n())
  

# To do -------------------------------------------------------------------

# ensure data is using the right units
# deal with censored data
# Complete char_converter with all possible variations
# handle data import colnames better
# Create process to review flagged data
# format Results_validated back into AWQMS format 
  #get rid of extra cols
