#This scriptis adaptedfrom the non-detect analysis scripts and is intended to serve as a starting point
#for IR analysis. Written using ecoli and Freshwater contact recreation standards



library(tidyverse)
library(readxl)
library(lubridate)

source("00 Functions.R")

#Import test dataset
ecoli_import <- read_excel("Bacteria/ecoli_AWQMS.xlsx") 


#create lists to get data out of for loops
geomeanlist = list()



# Data manipulation -------------------------------------------------------


#Mkaes the import col names work better with R
names(ecoli_import) <- make.names(names(ecoli_import), unique = TRUE, allow_ = TRUE)

ecoli <- non.detects(ecoli_import)

#Make table more manageable and then
#extract qual characters out of results field
ecoli <- ecoli %>%
  select(Monitoring.Location.ID, Activity.Start.Date,
         Activity.Start.Time, Activity.Type, Characteristic.Name,
         Result.Value, Result.Unit,
         Detection.Limit.Type1, Detection.Limit.Value1, r, qual) %>% 
  filter(!is.na(Result.Value)) %>%
  #set geomean standard and add detection limits to samples with missing detection limits
  mutate(strd = 126,
         Detection_limit = ifelse(is.na(Detection.Limit.Value1), r, Detection.Limit.Value1)) %>%
  #run through the scenarios
  mutate(r = ifelse(qual == "<", ifelse(strd < Detection_limit, strd/2, Detection_limit / 2), r)) %>%
  #create empty columns to put stuff into
  mutate(geomean = as.numeric(""))

  

# Geometric mean calculations --------------------------------------------


# Process the geometirc means
# These for loops first filter data down to individual monitoring stations
# and sets a variable for each sampling date that indicates the start of a 90 day geomean window.
# The second for loop loops through each activity date and creates a table of all activity dates in that
# 90 day window and calculates the geomettric mean. It then assigns the geomeans into the single location table
# created in the first loop, if there are more than 5 sampling dates in that window. 
# The end of the first loop puts the single location table into a list which is used to bring
# the data out of the for loop by binding it together after the loop into table "ecoli_geomean"


pb <- txtProgressBar(0, length(unique(ecoli$Monitoring.Location.ID)), style = 3)
for(i in 1:length(unique(ecoli$Monitoring.Location.ID))) {
  
  setTxtProgressBar(pb, i)
  #print(paste("Starting station", i, "of", length(unique(ecoli$Monitoring.Location.ID))))
  station <- unique(ecoli$Monitoring.Location.ID)[i]
  
  
  #filter table down to single station
  ecoli_single_station <- ecoli %>%
    filter(Monitoring.Location.ID == station) %>%
    #add the 90 day window
    mutate(geomean_start_date = as.Date(Activity.Start.Date)-90)
  
  
  for(j in 1:nrow(ecoli_single_station)) { #run through each row of single station table
    
    
    #start of 90 day window
    geomean_date <- ecoli_single_station$geomean_start_date[j]
    # end of 90 day window
    enddate <- ecoli_single_station$Activity.Start.Date[j]
    
    #create table for only samples in that window
    ecoli_period <- ecoli_single_station %>%
      filter(Activity.Start.Date <= enddate & Activity.Start.Date >= geomean_date )
    
      #get geomeans if number of unique days in that window is 5 or greater
    geom <- ifelse(length(unique(ecoli_period$Activity.Start.Date)) >= 5, geo_mean(ecoli_period$r), NA)
    
    #add these back into ecoli_single_station table
    ecoli_single_station[j,14] <-  as.numeric(geom)
 
    
  }
  

  #push single site data into a list for later binding 
  geomeanlist[[i]] <-  ecoli_single_station
  

} # Geometric Mean Calculations 

close(pb)
#bind list into dataframe
ecoli_geomean <- bind_rows(geomeanlist)



# Data analysis -----------------------------------------------------------


# map out where the excursions are
ecoli_geomean <- ecoli_geomean %>%
  arrange(Monitoring.Location.ID, Activity.Start.Date) %>%
  mutate(geom_exceed = ifelse(!is.na(geomean), ifelse(geomean > strd, 1, 0), 0),
         single_exceedance = ifelse(r > 406, 1, 0)) 



#method A sceario
ecoli_analysis <- ecoli_geomean %>%
  group_by(Monitoring.Location.ID) %>%
  summarise(num_geomean = sum(!is.na(geomean)),  #count of times the geomean was able to be calculated
            num_samples = n(),
            List = ifelse(num_samples > 5, #list if there are more than 5 samples and 
                          #(a geomean is above standard OR more than 2 single samples are more than single standard
                          ifelse(sum(geom_exceed) >= 1 | sum(single_exceedance) > 2, 1, 0),
                          #if less than 5 samples, list if (a geomean is above standard OR more than 1 single samples are more than single standard)
                          ifelse(sum(geom_exceed) >= 1 | sum(single_exceedance) > 1, 1, 0)),
           #Cat 3 if no geomeans are able to be calculated and no single sample excursion
            Cat_3 = ifelse(sum(num_geomean) == 0 & sum(single_exceedance) == 0, 1, 0),
           #Cat 3 b if no geomean and one sample is greater than single standard 
            Cat_3B = ifelse(sum(num_geomean) == 0 & sum(single_exceedance) == 1, 1, 0)
            )

View(ecoli_analysis)

