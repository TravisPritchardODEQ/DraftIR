library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)


#geomean function
geo_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#create function for listing requirements
funct_list <- function(value){
  if(value == 1){
    return(1)
  } else if(value >= 2 & value <= 18){
    return(2)
  } else if(value >= 19 & value <= 22){
    return(3)
  } else if(value >= 23 & value <= 35){
    return(4)
  } else if(value >= 36 & value <= 49){
    return (5)
  } else if(value >= 50 & value <= 63){
    return(6)
  } else if(value >= 64 & value <= 78){
    return(7)
  } else if(value >= 79 & value <= 92){
    return(8)
  } else if(value >= 93 & value <= 109){
    return(9)
  } else if(value >= 110 & value <= 125){
    return(10)
  } else if(value >= 126 & value <= 141){
    return(11)
  } else if(value >= 142 & value <= 158){
    return(12)
  } else if(value >= 159 & value <= 171){
    return(13)
  } else if(value >= 172 & value <= 191){
    return(14)
  } else if(value >= 192 & value <= 200){
    return(15)
  } else if(value >= 201 & value <= 250){
    return(16)
  }
}

#function for inserting listing requirements into table
funct_list_size <- function(df){
  for(i in 1:nrow(df)){
    df$num_to_list[i] <- as.numeric(funct_list(df$count[[i]])) 
  } 
  return(df)
}

#cete lists to get data out of for loops
geomeanlist = list()
method_D_list = list()


#Import test dataset
ecoli_import <- read_excel("ecoli_AWQMS.xlsx") 

#Mkaes the col names work better with R
names(ecoli_import) <- make.names(names(ecoli_import), unique = TRUE, allow_ = TRUE)

#Make table more manageable
#extract qual characters out of results field
ecoli <- ecoli_import %>%
  select(Monitoring.Location.ID, Activity.Start.Date,
         Activity.Start.Time, Activity.Type, Characteristic.Name,
         Result.Value, Result.Unit,
         Detection.Limit.Type1, Detection.Limit.Value1) %>% 
  mutate(qual = ifelse(str_sub(Result.Value, start = 1, end = 1) == "<" | str_sub(Result.Value, start = 1, end = 1) == ">" , 
                       str_sub(Result.Value, start = 1, end = 1), "=")) %>%
  #set data qualifiers
  mutate(r =ifelse(qual == "<" | qual == ">", str_sub(Result.Value, start = 2, end = length(Result.Value)), Result.Value )) %>%
  #change r to numeric
  mutate( r = as.numeric(r)) %>%
  #drop missing results
  filter(!is.na(Result.Value)) %>%
  #set geomean standard and add detection limits to samples with missing detection limits
  mutate(strd = 126,
         Detection_limit = ifelse(is.na(Detection.Limit.Value1), r, Detection.Limit.Value1)) %>%
  #run through the scenarios
  mutate(method_A = ifelse(qual == "<", Detection_limit, r),
         method_B = ifelse(qual == "<", Detection_limit/2, r),
         method_E = ifelse(qual == "<", ifelse(strd < Detection_limit, strd/2, Detection_limit / 2), r)
         #method_E = ifelse(qual == "<", strd/2, r)
                           ) %>%
  #create empty columns to put stuff into
  mutate(method_A_geom = "",
         method_B_geom = "",
         method_E_geom = "")

#remove non detects for that scenario
ecoli_method_D <- ecoli %>%
  filter(qual != "<")
  

  
  

#Process the geometirc means
for(i in 1:length(unique(ecoli$Monitoring.Location.ID))) {
  
  print(paste("Starting station", i, "of", length(unique(ecoli$Monitoring.Location.ID))))
  station <- unique(ecoli$Monitoring.Location.ID)[i]
  
  
  #filter table down to single station
  ecoli_full_dates <- ecoli %>%
    filter(Monitoring.Location.ID == station) %>%
    #add the 90 day window
    mutate(geomean_start_date = as.Date(Activity.Start.Date)-90)
  
  
  for(j in 1:nrow(ecoli_full_dates)) { #run through each row of single station table
    
    
    #start of 90 day window
    geomean_date <- ecoli_full_dates$geomean_start_date[j]
    # end of 90 day window
    enddate <- ecoli_full_dates$Activity.Start.Date[j]
    
    #create table for only samples in that window
    ecoli_period <- ecoli_full_dates %>%
      filter(Activity.Start.Date <= enddate & Activity.Start.Date >= geomean_date )
    
      #get geomeans if number of unique days in that window is 5 or greater
    geomean_A <- ifelse(length(unique(ecoli_period$Activity.Start.Date)) >= 5, geo_mean(as.numeric(ecoli_period$method_A)), NA)
    geomean_B <- ifelse(length(unique(ecoli_period$Activity.Start.Date)) >= 5, geo_mean(as.numeric(ecoli_period$method_B)), NA)
    geomean_E <- ifelse(length(unique(ecoli_period$Activity.Start.Date)) >= 5, geo_mean(as.numeric(ecoli_period$method_E)), NA)
    
    #add these back into ecoli_full_dates table
    ecoli_full_dates[j,17] <-  as.numeric(geomean_A)
    ecoli_full_dates[j,18] <-  as.numeric(geomean_B)
    ecoli_full_dates[j,19] <-  as.numeric(geomean_E)
    
  }

  #push single site data into a list for later binding 
  geomeanlist[[i]] <-  ecoli_full_dates
  

}

#bind list into dataframe
ecoli_geomean <- bind_rows(geomeanlist)




#Process the geometirc means for method D
for(k in 1:length(unique(ecoli_method_D$Monitoring.Location.ID))) {
  
  print(paste("Starting station", k, "of", length(unique(ecoli_method_D$Monitoring.Location.ID))))
  station <- unique(ecoli_method_D$Monitoring.Location.ID)[k]
  
  ecoli_full_dates <- ecoli_method_D %>%
    filter(Monitoring.Location.ID == station) %>%
    mutate(geomean_start_date = as.Date(Activity.Start.Date)-90)
  
  for(l in 1:nrow(ecoli_full_dates)) {
    
    geomean_date <- ecoli_full_dates$geomean_start_date[l]
    enddate <- ecoli_full_dates$Activity.Start.Date[l]
    
    ecoli_period <- ecoli_full_dates %>%
      filter(Activity.Start.Date <= enddate & Activity.Start.Date >= geomean_date )
    
    geomean_A <- ifelse(length(unique(ecoli_period$Activity.Start.Date)) >= 5, geo_mean(as.numeric(ecoli_period$method_A)), NA)
    geomean_B <- ifelse(length(unique(ecoli_period$Activity.Start.Date)) >= 5, geo_mean(as.numeric(ecoli_period$method_B)), NA)
    geomean_E <- ifelse(length(unique(ecoli_period$Activity.Start.Date)) >= 5, geo_mean(as.numeric(ecoli_period$method_E)), NA)
    
    ecoli_full_dates[l,17] <-  as.numeric(geomean_A)
    ecoli_full_dates[l,18] <-  as.numeric(geomean_B)
    ecoli_full_dates[l,19] <-  as.numeric(geomean_E)
    
  }
  
  method_D_list[[k]] <-  ecoli_full_dates
  
  
}



ecoli_geomean_method_D <- bind_rows( method_D_list)


#this part is a mess and I hate it. 

ecoli_geomean <- ecoli_geomean %>%
  arrange(Monitoring.Location.ID, Activity.Start.Date) %>%
  mutate(method_A_geom = as.numeric(method_A_geom),
         method_B_geom = as.numeric(method_B_geom),
         method_E_geom = as.numeric(method_E_geom))

#check to see if geomeans are above standard
ecoli_scenarios <- ecoli_geomean %>%
  mutate(method_A_geom_exceed = ifelse(!is.na(method_A_geom), ifelse(method_A_geom > strd, 1, 0), 0),
         method_B_geom_exceed = ifelse(!is.na(method_B_geom), ifelse(method_B_geom > strd, 1, 0), 0),
         method_E_geom_exceed = ifelse(!is.na(method_E_geom), ifelse(method_E_geom > strd, 1, 0), 0))

#Check to see if single sample results are above single sample strd

ecoli_scenarios <- ecoli_scenarios %>%
  mutate(method_A_single_exceedance = ifelse(method_A > 406, 1, 0),
         method_B_single_exceedance = ifelse(method_B > 406, 1, 0),
         method_E_single_exceedance = ifelse(method_E > 406, 1, 0))

#table for max values by station for merging back into table. This is
#to compare against single sample standard
# ecoli_max <- ecoli_geomean %>%
#   group_by(Monitoring.Location.ID) %>%
#   summarise(max = max(r))
# 
# #join max values back in
# ecoli_scenarios <-  ecoli_scenarios %>%
#   left_join(ecoli_max, by = "Monitoring.Location.ID")
ecoli_method_D_scenarios <- ecoli_geomean_method_D %>%
  mutate(method_A_geom = as.numeric(method_A_geom),
         method_B_geom = as.numeric(method_B_geom),
        method_E_geom = as.numeric(method_E_geom)) %>%
  mutate(method_A_geom_exceed = ifelse(!is.na(method_A_geom), ifelse(method_A_geom > strd, 1, 0), 0),
         method_B_geom_exceed = ifelse(!is.na(method_B_geom), ifelse(method_B_geom > strd, 1, 0), 0),
         method_E_geom_exceed = ifelse(!is.na(method_E_geom), ifelse(method_E_geom > strd, 1, 0), 0))

ecoli_method_D_scenarios <- ecoli_method_D_scenarios %>%
  mutate(method_A_single_exceedance = ifelse(method_A > 406, 1, 0),
         method_B_single_exceedance = ifelse(method_B > 406, 1, 0),
         method_E_single_exceedance = ifelse(method_E > 406, 1, 0))

# ecoli_max_D <- ecoli_geomean_method_D %>%
#   group_by(Monitoring.Location.ID) %>%
#   summarise(max = max(r))


# ecoli_method_D_scenarios <-  ecoli_method_D_scenarios %>%
#   left_join(ecoli_max_D, by = "Monitoring.Location.ID")

#method A sceario
tbl_method_A_ecoli <- ecoli_scenarios %>%
  group_by(Monitoring.Location.ID) %>%
  summarise(num_geomean = sum(!is.na(method_A_geom)),  #count of times the geomean was able to be calculated
            List = ifelse(n() > 5, #list if there are more than 5 samples and 
                          #(a geomean is above standard OR more than 2 single samples are more than single standard
                          ifelse(sum(method_A_geom_exceed) >= 1 | sum(method_A_single_exceedance) > 2, 1, 0),
                          #if less than 5 samples, list if (a geomean is above standard OR more than 1 single samples are more than single standard)
                          ifelse(sum(method_A_geom_exceed) >= 1 | sum(method_A_single_exceedance) > 1, 1, 0)),
           #Cat 3 if no geomeans are able to be calculated and no single sample excursion
            Cat_3 = ifelse(sum(num_geomean) == 0 & sum(method_A_single_exceedance) == 0, 1, 0),
           #Cat 3 b if no geomean and one sanple is greater than single standard 
            Cat_3B = ifelse(sum(num_geomean) == 0 & sum(method_A_single_exceedance) == 1, 1, 0)
            )

tbl_method_B_ecoli <- ecoli_scenarios %>%
  group_by(Monitoring.Location.ID) %>%
  summarise(num_geomean = sum(!is.na(method_B_geom)),  #count of times the geomean was able to be calculated
            List = ifelse(n() > 5, #list if there are more than 5 samples and 
                          #(a geomean is above standard OR more than 2 single samples are more than single standard
                          ifelse(sum(method_B_geom_exceed) >= 1 | sum(method_B_single_exceedance) > 2, 1, 0),
                          #if less than 5 samples, list if (a geomean is above standard OR more than 1 single samples are more than single standard)
                          ifelse(sum(method_B_geom_exceed) >= 1 | sum(method_B_single_exceedance) > 1, 1, 0)),
            #Cat 3 if no geomeans are able to be calculated and no single sample excursion
            Cat_3 = ifelse(sum(num_geomean) == 0 & sum(method_B_single_exceedance) == 0, 1, 0),
            #Cat 3 b if no geomean and one sanple is greater than single standard 
            Cat_3B = ifelse(sum(num_geomean) == 0 & sum(method_B_single_exceedance) == 1, 1, 0)
  )

tbl_method_E_ecoli <- ecoli_scenarios %>%
  group_by(Monitoring.Location.ID) %>%
  summarise(num_geomean = sum(!is.na(method_E_geom)),  #count of times the geomean was able to be calculated
            List = ifelse(n() > 5, #list if there are more than 5 samples and 
                          #(a geomean is above standard OR more than 2 single samples are more than single standard
                          ifelse(sum(method_E_geom_exceed) >= 1 | sum(method_E_single_exceedance) > 2, 1, 0),
                          #if less than 5 samples, list if (a geomean is above standard OR more than 1 single samples are more than single standard)
                          ifelse(sum(method_E_geom_exceed) >= 1 | sum(method_E_single_exceedance) > 1, 1, 0)),
            #Cat 3 if no geomeans are able to be calculated and no single sample excursion
            Cat_3 = ifelse(sum(num_geomean) == 0 & sum(method_E_single_exceedance) == 0, 1, 0),
            #Cat 3 b if no geomean and one sanple is greater than single standard 
            Cat_3B = ifelse(sum(num_geomean) == 0 & sum(method_E_single_exceedance) == 1, 1, 0)
  )


tbl_method_D_ecoli <- ecoli_method_D_scenarios %>%
  group_by(Monitoring.Location.ID) %>%
  summarise(num_geomean = sum(!is.na(method_E_geom)),  #count of times the geomean was able to be calculated
            List = ifelse(n() > 5, #list if there are more than 5 samples and 
                          #(a geomean is above standard OR more than 2 single samples are more than single standard
                          ifelse(sum(method_E_geom_exceed) >= 1 | sum(method_E_single_exceedance) > 2, 1, 0),
                          #if less than 5 samples, list if (a geomean is above standard OR more than 1 single samples are more than single standard)
                          ifelse(sum(method_E_geom_exceed) >= 1 | sum(method_E_single_exceedance) > 1, 1, 0)),
            #Cat 3 if no geomeans are able to be calculated and no single sample excursion
            Cat_3 = ifelse(sum(num_geomean) == 0 & sum(method_E_single_exceedance) == 0, 1, 0),
            #Cat 3 b if no geomean and one sanple is greater than single standard 
            Cat_3B = ifelse(sum(num_geomean) == 0 & sum(method_E_single_exceedance) == 1, 1, 0)
  )

cnames <- c("method", "sites",  'List', 'Cat_3', "Cat_3b")

scenarios <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(scenarios) <- cnames

scenarios[1,1] = "Use Detection Limit"
scenarios[1,2] = nrow(tbl_method_A_ecoli)
scenarios[1,3] = sum(tbl_method_A_ecoli$List)
scenarios[1,4] = sum(tbl_method_A_ecoli$Cat_3)
scenarios[1,5] = sum(tbl_method_A_ecoli$Cat_3B)


scenarios[2,1] = "Use 1/2 Detection Limit"
scenarios[2,2] = nrow(tbl_method_B_ecoli)
scenarios[2,3] = sum(tbl_method_B_ecoli$List)
scenarios[2,4] = sum(tbl_method_B_ecoli$Cat_3)
scenarios[2,5] = sum(tbl_method_B_ecoli$Cat_3B)



scenarios[3,1] = "Set to 0"
scenarios[3,2] = "N/A"
scenarios[3,3] = "N/A"
scenarios[3,4] = "N/A"
scenarios[3,5] = "N/A"


scenarios[4,1] = "Eliminate"
scenarios[4,2] = nrow(tbl_method_D_ecoli)
scenarios[4,3] = sum(tbl_method_D_ecoli$List)
scenarios[4,4] = sum(tbl_method_D_ecoli$Cat_3)
scenarios[4,5] = sum(tbl_method_D_ecoli$Cat_3B)

scenarios[5,1] = "Use 1/2 Criteria or 1/2 QL"
scenarios[5,2] = nrow(tbl_method_E_ecoli)
scenarios[5,3] = sum(tbl_method_E_ecoli$List)
scenarios[5,4] = sum(tbl_method_E_ecoli$Cat_3)
scenarios[5,5] = sum(tbl_method_E_ecoli$Cat_3B)


write.csv(scenarios, "Censored_data_scenarios_ecoli.csv", row.names = FALSE)


#test to see how geomeans differ between A and E
test <- ecoli_geomean %>%
  filter(method_A_geom != method_E_geom)