library(readxl)
library(tidyverse)
library(stringr)
library(psych)
library(dataRetrieval)
library(SciViews)
library(lubridate)

#disable scientific notation
options(scipen = 999)


# script to run through different scenarios for dealing with non detect data 

# method A is to use detection limit
# method B is to use 1/2 detection limit
# method C is to use 0
# method D is to eliminate
# method E to to use 1/2 standard



#Import test dataset
toxics_import <- read_excel("E:/Integrated Report/Nondetects/AWQMS_LowCriteria_ToxicsData.xlsx") 



#give import valid r names
colnames(toxics_import) <- make.names(colnames(toxics_import), unique=TRUE)

#import WQX lead data
WQX_lead <- read.csv("WQX Portal Lead.csv", stringsAsFactors = FALSE)

WQX_alka <- readWQPdata(statecode="US:41",
                        characteristicName="Alkalinity, total")

WQX_ecoli <- readWQPdata(statecode="US:41",
                         characteristicName="Escherichia coli")

WQX_ecoli_sites <- whatWQPsites(statecode="US:41",
                        characteristicName="Escherichia coli")

wqx_alka_water <- WQX_alka %>%
  filter(ActivityMediaName == "Water",
         ActivityMediaSubdivisionName == "Surface Water") 

wqz_lead_water <- WQX_lead %>%
  mutate(ActivityStartDate = ymd(ActivityStartDate)) %>%
  filter(ActivityMediaName == "Water",
         ActivityMediaSubdivisionName == "Surface Water",
         ProjectIdentifier != "Landfill") %>%
  filter(ActivityStartDate > ymd("2008-01-01"),
         ActivityTypeCode == "Sample-Routine")

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

#geometric mean function
geo_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#lead acute dissolved standard function
dis_lead_actute_strd = function(x){
  exp(1.273*(log(x)) -1.46)
}

tot_lead_acute_strd = function(x){

  exp(1.273*(log(x)) -1.46)*(1.46203-(log(x))*(0.145712))
}

#lead acute dissolved standard function
dis_lead_chron_strd = function(x){
  exp(1.273*(log(x)) - 4.705)
}

tot_lead_chron_strd = function(x){
  
  exp(1.273*(log(x)) - 4.705)*(1.46203-(log(x))*(0.145712))
}   

 
#Create dataframe for standards
char <- c("p,p'-DDT")
strd <- c(0.000022)
t_standards <- data.frame(char, strd, stringsAsFactors = FALSE)



#Create table
toxics_data <- toxics_import %>%
  filter(Activity.Media == "Water",
         Monitoring.Location.Type != "Storm Sewer",
         Monitoring.Location.Type != "Well") %>%
  filter(Characteristic.Name == "p,p'-DDT") %>%
  #set data qualifier
  mutate(qual = ifelse(str_sub(Result.Value, start = 1, end = 1) == "<", str_sub(Result.Value, start = 1, end = 1), "=")) %>%
  #set data qualifiers
  mutate(r =as.numeric(ifelse(qual == "<", str_sub(Result.Value, start = 2, end = length(Result.Value)), Result.Value )))  %>%
  #Set units to ug/l from ng/l to match standards
  mutate(r = ifelse(Result.Unit == "ng/l", r/1000, r),
         r_units = ifelse(Result.Unit == "ng/l", "ug/l", Result.Unit)) %>%
  mutate(Detection_limit = ifelse(Detection.Limit.Unit1 == "ng/l", Detection.Limit.Value1/1000, Detection.Limit.Value1)) %>%
  #Add standards to table
  left_join(t_standards, by = c("Characteristic.Name" = "char")) %>%
  #Run through different methods
  mutate(method_A = ifelse(qual == "<", Detection_limit, r),
         method_B = ifelse(qual == "<", Detection_limit/2, r),
         method_C = ifelse(qual == "<", 0, r),
         method_E = ifelse(qual == "<", strd/2, r)) %>%
  mutate(method_A_exceed = ifelse(method_A > strd, 1, 0),
         method_B_exceed = ifelse(method_B > strd, 1, 0),
         method_C_exceed = ifelse(method_C > strd, 1, 0),
         method_E_exceed = ifelse(method_E > strd, 1, 0))



# Run through the different scenarios using binomial -------------------------------------
#Create tables for the different methods
tbl_method_A <- toxics_data %>%
  group_by(Monitoring.Location.ID, Characteristic.Name) %>% 
  summarise(exceed = sum(method_A_exceed),
            count = as.numeric(n()),
            percent = round(exceed / count * 100, digits = 0)) %>%
  ungroup() %>%
  mutate(num_to_list = "")

tbl_method_A <- funct_list_size(tbl_method_A)

tbl_method_A <- tbl_method_A %>%
  mutate(num_to_list = as.numeric(num_to_list)) %>%
  mutate(list = ifelse(exceed >= num_to_list, 1, 0))


tbl_method_B <- toxics_data %>%
  group_by(Monitoring.Location.ID, Characteristic.Name) %>% 
  summarise(exceed = sum(method_B_exceed),
            count = n(),
            percent = round(exceed / count * 100, digits = 0))%>%
  ungroup() %>%
  mutate(num_to_list = "")

tbl_method_B <- funct_list_size(tbl_method_B)

tbl_method_B <- tbl_method_B %>%
  mutate(num_to_list = as.numeric(num_to_list)) %>%
  mutate(list = ifelse(exceed >= num_to_list, 1, 0))

tbl_method_C <- toxics_data %>%
  group_by(Monitoring.Location.ID, Characteristic.Name) %>% 
  summarise(exceed = sum(method_C_exceed),
            count = n(),
            percent = round(exceed / count * 100, digits = 0))%>%
  ungroup() %>%
  mutate(num_to_list = "")

tbl_method_C <- funct_list_size(tbl_method_C)

tbl_method_C <- tbl_method_C %>%
  mutate(num_to_list = as.numeric(num_to_list)) %>%
  mutate(list = ifelse(exceed >= num_to_list, 1, 0))

tbl_method_E <- toxics_data %>%
  group_by(Monitoring.Location.ID, Characteristic.Name) %>% 
  summarise(exceed = sum(method_E_exceed),
            count = n(),
            percent = round(exceed / count * 100, digits = 0))%>%
  ungroup() %>%
  mutate(num_to_list = "")

tbl_method_E <- funct_list_size(tbl_method_E)

tbl_method_E <- tbl_method_E %>%
  mutate(num_to_list = as.numeric(num_to_list)) %>%
  mutate(list = ifelse(exceed >= num_to_list, 1, 0))

method_D <- toxics_data %>%
  filter(method_C != 0)

tbl_method_D <- method_D %>%
  group_by(Monitoring.Location.ID, Characteristic.Name) %>% 
  summarise(exceed = sum(method_C_exceed),
            count = n(),
            percent = round(exceed / count * 100, digits = 0)) %>%
  ungroup() %>%
  mutate(num_to_list = "")

tbl_method_D <- funct_list_size(tbl_method_D)

tbl_method_D <- tbl_method_D %>%
  mutate(num_to_list = as.numeric(num_to_list)) %>%
  mutate(list = ifelse(exceed >= num_to_list, 1, 0))


# Sum it all up -----------------------------------------------------------
#This is probablly the dumbest way to add all the scenarios back into a summary table,
#but i did it this way anyway. 

cnames <- c("method", "sites", "percent_samp_exceed", 'num_sites_listed', "percent_sites_listed")

scenarios <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(scenarios) <- cnames

scenarios[1,1] = "Use Detection Limit"
scenarios[1,2] = nrow(tbl_method_A)
scenarios[1,3] = round(sum(tbl_method_A$exceed)/sum(tbl_method_A$count) * 100, digits = 2)
scenarios[1,4] = sum(tbl_method_A$list)
scenarios[1,5] = round(sum(tbl_method_A$list)/nrow(tbl_method_A) * 100, digits = 2)

scenarios[2,1] = "Use 1/2 Detection Limit"
scenarios[2,2] = nrow(tbl_method_B)
scenarios[2,3] = round(sum(tbl_method_B$exceed)/sum(tbl_method_B$count) * 100, digits = 2)
scenarios[2,4] = sum(tbl_method_B$list)
scenarios[2,5] = round(sum(tbl_method_B$list)/nrow(tbl_method_B) * 100, digits = 2)

scenarios[3,1] = "Set to 0"
scenarios[3,2] = nrow(tbl_method_C)
scenarios[3,3] = round(sum(tbl_method_C$exceed)/sum(tbl_method_C$count) * 100, digits = 2)
scenarios[3,4] = sum(tbl_method_C$list)
scenarios[3,5] = round(sum(tbl_method_C$list)/nrow(tbl_method_C) * 100, digits = 2)

scenarios[4,1] = "Eliminate"
scenarios[4,2] = nrow(tbl_method_D)
scenarios[4,3] = round(sum(tbl_method_D$exceed)/sum(tbl_method_D$count) * 100, digits = 2)
scenarios[4,4] = sum(tbl_method_D$list)
scenarios[4,5] = round(sum(tbl_method_D$list)/nrow(tbl_method_D) * 100, digits = 2)

scenarios[5,1] = "Use 1/2 Standard"
scenarios[5,2] = nrow(tbl_method_E)
scenarios[5,3] = round(sum(tbl_method_E$exceed)/sum(tbl_method_E$count) * 100, digits = 2)
scenarios[5,4] = sum(tbl_method_E$list)
scenarios[5,5] = round(sum(tbl_method_E$list)/nrow(tbl_method_E) * 100, digits = 2)

#write.csv(scenarios, "E:/Integrated Report/Nondetects/censored_data_scenarios.csv", row.names = FALSE)



# Run through scenarios using geomeans ------------------------------------

geomean <- toxics_data %>%
  ungroup() %>%
  group_by(Monitoring.Location.ID, Characteristic.Name, strd) %>%
  summarise(A_geomean = geo_mean(method_A),
            B_geomean = geo_mean(method_B),
            C_geomean = geo_mean(method_C),
            E_geomean = geo_mean(method_E),
            num_samples = n(),
  ) %>%
  mutate(method_A_exceed = ifelse(A_geomean > strd, 1, 0),
         method_B_exceed = ifelse(B_geomean > strd, 1, 0),
         method_C_exceed = ifelse(C_geomean > strd, 1, 0),
         method_E_exceed = ifelse(E_geomean > strd, 1, 0)) 


geomean_summary <- geomean %>%
  ungroup() %>%
  summarise(sites = n(),
            A_num_sites_listed = sum(method_A_exceed),
            A_percent_sites_listed = round(sum(method_A_exceed)/ n() * 100, digits = 2),
            B_num_sites_listed = sum(method_B_exceed),
            B_percent_sites_listed = round(sum(method_B_exceed)/ n() * 100, digits = 2),
            E_num_sites_listed = sum(method_E_exceed),
            E_percent_sites_listed = round(sum(method_E_exceed)/ n() * 100, digits = 2))

geo_mean_eliminate <- toxics_data %>%
  ungroup() %>%
  filter(qual != "<") %>%
  group_by(Monitoring.Location.ID, strd) %>%
  summarise(D_geomean = geo_mean(r)) %>%
  mutate(method_D_exceed = ifelse(D_geomean > strd, 1, 0))


# Get data from geomean listed sites ----------------------------------------------

geo_mean_sites <- geo_mean_eliminate$Monitoring.Location.ID

geo_mean_data <- toxics_data %>%
  filter(Monitoring.Location.ID %in% geo_mean_sites) %>%
  select(Monitoring.Location.ID, 
         Monitoring.Location.Latitude, 
         Monitoring.Location.Longitude, 
         qual, 
         r, 
         r_units,
         Detection_limit,
         strd,
         method_A,
         method_B,
         method_C, 
         method_E) %>%
  rename("Set_detection_limit" = method_A,
         "1/2_Det_lim" = method_B,
         "Set to 0" =method_C,
         "Set_1/2_crit" = method_E) %>%
  arrange(Monitoring.Location.ID) %>%
  group_by(Monitoring.Location.ID) %>%
  mutate(num_samples = n())


#write.csv(geo_mean_data, "Sites_with_detects.csv", row.names = FALSE)  
  
#Again, this is probablly the dumbest way to add all the scenarios back into a summary table,
#but i did it this way anyway. 

geo_cnames <- c("method", "sites", 'num_sites_listed', "percent_sites_listed")

geo_mean_scenarios <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(geo_mean_scenarios) <- geo_cnames

geo_mean_scenarios[1,1] = "Use Detection Limit"
geo_mean_scenarios[1,2] = geomean_summary$sites[1]
geo_mean_scenarios[1,3] = geomean_summary$A_num_sites_listed[1]
geo_mean_scenarios[1,4] = geomean_summary$A_percent_sites_listed[1]


geo_mean_scenarios[2,1] = "Use 1/2 Detection Limit"
geo_mean_scenarios[2,2] = geomean_summary$sites[1]
geo_mean_scenarios[2,3] = geomean_summary$B_num_sites_listed[1]
geo_mean_scenarios[2,4] = geomean_summary$B_percent_sites_listed[1]

geo_mean_scenarios[3,1] = "Set to 0"
geo_mean_scenarios[3,2] = "-"
geo_mean_scenarios[3,3] = "Unable to calculate geomeans with 0"
geo_mean_scenarios[3,4] = "-"


geo_mean_scenarios[4,1] = "Eliminate"
geo_mean_scenarios[4,2] = nrow(geo_mean_eliminate)
geo_mean_scenarios[4,3] = sum(geo_mean_eliminate$method_D_exceed)
geo_mean_scenarios[4,4] = round(sum(geo_mean_eliminate$method_D_exceed)/nrow(geo_mean_eliminate)*100, digits = 2)


geo_mean_scenarios[5,1] = "Use 1/2 Standard"
geo_mean_scenarios[5,2] = geomean_summary$sites[1]
geo_mean_scenarios[5,3] = geomean_summary$E_num_sites_listed[1]
geo_mean_scenarios[5,4] = geomean_summary$E_percent_sites_listed[1]

#write.csv(geo_mean_scenarios, "censored_data_scenarios_geomean.csv", row.names = FALSE)


# Lead data from WQX ------------------------------------------------------

alka_data <- wqx_alka_water %>%
  filter(ResultSampleFractionText == "Total",
         ActivityTypeCode == "Sample-Routine") %>%
  arrange(MonitoringLocationIdentifier, ActivityStartDate, ActivityStartTime.Time) %>%
  select(OrganizationIdentifier,
         ActivityIdentifier,
         ActivityMediaName,
         ActivityStartDate,
         ActivityStartTime.Time,
         MonitoringLocationIdentifier,
         ResultMeasureValue,
         ResultMeasure.MeasureUnitCode,
         MeasureQualifierCode
         )

alka_test <- alka_data %>%
  filter(MonitoringLocationIdentifier == "OREGONDEQ-11856-ORDEQ")

#combine lead data with alkalinity data and run through scenarios 
lead_type_count <- wqz_lead_water %>%
  group_by(MonitoringLocationIdentifier,ActivityStartDate,ActivityStartTime.Time) %>%
  summarise(num = n()) 

lead_type_remove <- wqz_lead_water %>%
  ungroup() %>%
  left_join(lead_type_count, by = c("MonitoringLocationIdentifier","ActivityStartDate","ActivityStartTime.Time")) %>%
  mutate(dup_process = ifelse(num == 2 & ResultSampleFractionText == "Total Recoverable", 
                              "Delete",
                              ifelse(num == 2 & ResultSampleFractionText == "Recoverable", 
                                     "Delete", 
                                     "Keep"))) %>%
  filter(dup_process == "Keep") %>%
  select(-num, -dup_process)
                             
  

#this table does all the heavy lifting for the lead data
lead_data <- lead_type_remove %>%
  #join the hardness data
  left_join(alka_data, by = c("ActivityIdentifier", 
                              "ActivityIdentifier", "ActivityStartDate", "ActivityStartTime.Time")
  ) %>%
  #remove datapoints with no heardness
  filter(!is.na(ResultMeasureValue.y)) %>%
  # remove mg/kg and suspended 
  filter(ResultMeasure.MeasureUnitCode.x != "mg/kg",
         ResultSampleFractionText != "Suspended") %>%
  # set the standard based on hardness
  mutate(strd = ifelse(ResultSampleFractionText == "Dissolved", 
                       dis_lead_chron_strd(ResultMeasureValue.y), 
                       tot_lead_chron_strd(ResultMeasureValue.y))) %>%
  # make the data qualifiers easier to deal with
  mutate(detect = ifelse(ResultDetectionConditionText == "Not Detected", "Non Detect",""),
         quant = ifelse(ResultValueTypeName == "Estimated", "Est", ""),
         qual = ifelse(str_sub(ResultMeasureValue.x, start = 1, end = 1) == "<", 
                       str_sub(ResultMeasureValue.x, start = 1, end = 1), 
                       ifelse(detect == "Non Detect", "<", 
                              ifelse(quant == "Est", "Est", "="))),
         r = as.numeric(ifelse(str_sub(ResultMeasureValue.x, start = 1, end = 1) == "<", str_sub(ResultMeasureValue.x, start = 2, end = length(ResultMeasureValue.x)),
                               ifelse(detect == "Non Detect",DetectionQuantitationLimitMeasure.MeasureValue, ResultMeasureValue.x )))) %>%
  # run through the methods
  mutate(method_A = ifelse(qual == "<" , 
                           DetectionQuantitationLimitMeasure.MeasureValue,
                           ResultMeasureValue.x),
         method_B = ifelse(qual == "<" , 
                           DetectionQuantitationLimitMeasure.MeasureValue / 2,
                           ResultMeasureValue.x),
         method_C = ifelse(qual == "<" , 
                           0,
                           ResultMeasureValue.x),
         method_E = ifelse(qual == "<" ,
                           ifelse(DetectionQuantitationLimitMeasure.MeasureValue > strd, format(round(strd / 2, 2), nsmall = 2),
                                  DetectionQuantitationLimitMeasure.MeasureValue /2),
                           ResultMeasureValue.x)
  ) %>%
  #narrow table to make a bit easier to deal with
  select(OrganizationIdentifier.x, 
         ActivityIdentifier,
         ActivityStartDate,
         ActivityStartTime.Time,
         MonitoringLocationIdentifier.x,
         CharacteristicName,
         ResultMeasureValue.x,
         ResultMeasure.MeasureUnitCode.x,
         MeasureQualifierCode.x,
         ResultDetectionConditionText,
         ResultValueTypeName,
         ResultLaboratoryCommentText,
         DetectionQuantitationLimitTypeName,
         DetectionQuantitationLimitMeasure.MeasureValue,
         DetectionQuantitationLimitMeasure.MeasureUnitCode,
         ResultMeasureValue.y,
         ResultMeasure.MeasureUnitCode.y,
         MeasureQualifierCode.y,
         ResultSampleFractionText,
         strd,
         detect,
         quant,
         qual,
         method_A,
         method_B,
         method_C,
         method_E) %>%
  # get rid of confusing location IDs
  rename(MonitoringLocationIdentifier = MonitoringLocationIdentifier.x) %>%
  # make qual inclusive of all qualifications
  mutate(qual = ifelse(detect == "Non Detect", "<", qual)) %>%
  # compare methods to standard
  mutate(method_A_exceed = ifelse(method_A > strd, 1, 0),
         method_B_exceed = ifelse(method_B > strd, 1, 0),
         method_C_exceed = ifelse(method_C > strd, 1, 0),
         method_E_exceed = ifelse(method_E > strd, 1, 0)) 

#run through scenarios using binomial
tbl_method_A_lead <- lead_data %>%
  group_by(MonitoringLocationIdentifier, CharacteristicName) %>% 
  summarise(tot_exceed = sum(method_A_exceed),
            est_exceed = sum(qual == "Est" & method_A_exceed == 1),
            act_exceed = sum(qual == "=" & method_A_exceed == 1),
            ND_exceed = sum(qual == "<" & method_A_exceed == 1),
            count = as.numeric(n()),
            tot_est = sum(qual == "Est"),
            percent = round(tot_exceed / count * 100, digits = 0)) %>%
  ungroup() %>%
  mutate(num_to_list = "")

tbl_method_A_lead <- funct_list_size(tbl_method_A_lead)

tbl_method_A_lead <- tbl_method_A_lead %>%
  mutate(num_to_list = as.numeric(num_to_list)) %>%
  mutate(category = ifelse(act_exceed >= num_to_list, "List",
                           ifelse(tot_exceed >= num_to_list & act_exceed < num_to_list, "Cat_3B", "Cat_2")))

tbl_method_A_lead_sum <- tbl_method_A_lead %>%
  group_by(category) %>%
  summarise(tot_cat = n()) %>%
  spread(category, tot_cat) %>%
  mutate(Cat_3B = ifelse("Cat_3B" %in% names(.), Cat_3B, 0)) 

tbl_method_B_lead <- lead_data %>%
  group_by(MonitoringLocationIdentifier, CharacteristicName) %>% 
  summarise(tot_exceed = sum(method_B_exceed),
            est_exceed = sum(qual == "Est" & method_B_exceed == 1),
            act_exceed = sum(qual == "=" & method_B_exceed == 1),
            ND_exceed = sum(qual == "<" & method_B_exceed == 1),
            count = as.numeric(n()),
            tot_est = sum(qual == "Est"),
            percent = round(tot_exceed / count * 100, digits = 0)) %>%
  ungroup() %>%
  mutate(num_to_list = "")

tbl_method_B_lead <- funct_list_size(tbl_method_B_lead)

tbl_method_B_lead <- tbl_method_B_lead %>%
  mutate(num_to_list = as.numeric(num_to_list)) %>%
  mutate(category = ifelse(act_exceed >= num_to_list, "List",
                           ifelse(tot_exceed >= num_to_list & act_exceed < num_to_list, "Cat_3B", "Cat_2")))

tbl_method_B_lead_sum <- tbl_method_B_lead %>%
  group_by(category) %>%
  summarise(tot_cat = n()) %>%
  spread(category, tot_cat) %>%
  mutate(Cat_3B = ifelse("Cat_3B" %in% names(.), Cat_3B, 0)) 



tbl_method_C_lead <- lead_data %>%
  group_by(MonitoringLocationIdentifier, CharacteristicName) %>% 
  summarise(tot_exceed = sum(method_C_exceed),
            est_exceed = sum(qual == "Est" & method_C_exceed == 1),
            act_exceed = sum(qual == "=" & method_C_exceed == 1),
            ND_exceed = sum(qual == "<" & method_C_exceed == 1),
            count = as.numeric(n()),
            tot_est = sum(qual == "Est"),
            percent = round(tot_exceed / count * 100, digits = 0)) %>%
  ungroup() %>%
  mutate(num_to_list = "")

tbl_method_C_lead <- funct_list_size(tbl_method_C_lead)

tbl_method_C_lead <- tbl_method_C_lead %>%
  mutate(num_to_list = as.numeric(num_to_list)) %>%
  mutate(category = ifelse(act_exceed >= num_to_list, "List",
                           ifelse(tot_exceed >= num_to_list & act_exceed < num_to_list, "Cat_3B", "Cat_2")))

tbl_method_C_lead_sum <- tbl_method_C_lead %>%
  group_by(category) %>%
  summarise(tot_cat = n()) %>%
  spread(category, tot_cat) %>%
  mutate(Cat_3B = ifelse("Cat_3B" %in% names(.), Cat_3B, 0))  


tbl_method_E_lead <- lead_data %>%
  group_by(MonitoringLocationIdentifier, CharacteristicName) %>% 
  summarise(tot_exceed = sum(method_E_exceed),
            est_exceed = sum(qual == "Est" & method_E_exceed == 1),
            act_exceed = sum(qual == "=" & method_E_exceed == 1),
            ND_exceed = sum(qual == "<" & method_E_exceed == 1),
            count = as.numeric(n()),
            tot_est = sum(qual == "Est"),
            percent = round(tot_exceed / count * 100, digits = 0)) %>%
  ungroup() %>%
  mutate(num_to_list = "")

tbl_method_E_lead <- funct_list_size(tbl_method_E_lead)

tbl_method_E_lead <- tbl_method_E_lead %>%
  mutate(num_to_list = as.numeric(num_to_list)) %>%
  mutate(category = ifelse(act_exceed >= num_to_list, "List",
                           ifelse(tot_exceed >= num_to_list & act_exceed < num_to_list, "Cat_3B", "Cat_2")))

tbl_method_E_lead_sum <- tbl_method_E_lead %>%
  group_by(category) %>%
  summarise(tot_cat = n()) %>%
  spread(category, tot_cat) %>%
  mutate(Cat_3B = ifelse("Cat_3B" %in% names(.), Cat_3B, 0)) 

lead_method_D <- lead_data %>%
  filter(method_C != 0)

tbl_method_D_lead <- lead_method_D %>%
  group_by(MonitoringLocationIdentifier, CharacteristicName) %>% 
  summarise(tot_exceed = sum(method_B_exceed),
            est_exceed = sum(qual == "Est" & method_B_exceed == 1),
            act_exceed = sum(qual == "=" & method_B_exceed == 1),
            ND_exceed = sum(qual == "<" & method_B_exceed == 1),
            count = as.numeric(n()),
            tot_est = sum(qual == "Est"),
            percent = round(tot_exceed / count * 100, digits = 0)) %>%
  ungroup() %>%
  mutate(num_to_list = "")

tbl_method_D_lead <- funct_list_size(tbl_method_D_lead)

tbl_method_D_lead <- tbl_method_D_lead %>%
  mutate(num_to_list = as.numeric(num_to_list)) %>%
  mutate(category = ifelse(act_exceed >= num_to_list, "List",
                           ifelse(tot_exceed >= num_to_list & act_exceed < num_to_list, "Cat_3B", "Cat_2")))

tbl_method_D_lead_sum <- tbl_method_D_lead %>%
  group_by(category) %>%
  summarise(tot_cat = n()) %>%
  spread(category, tot_cat) %>%
  mutate(Cat_3B = ifelse("Cat_3B" %in% names(.), Cat_3B, 0)) 


cnames <- c("method", "sites", "Category 2", 'Category 3b', "List")

lead_scenarios <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(lead_scenarios) <- cnames

lead_scenarios[1,1] = "Use Detection Limit"
lead_scenarios[1,2] = nrow(tbl_method_A_lead)
lead_scenarios[1,3] = tbl_method_A_lead_sum$Cat_2[[1]]
lead_scenarios[1,4] = tbl_method_A_lead_sum$Cat_3B[[1]]
lead_scenarios[1,5] = tbl_method_A_lead_sum$List[[1]]

lead_scenarios[2,1] = "Use 1/2 Detection Limit"
lead_scenarios[2,2] = nrow(tbl_method_B_lead)
lead_scenarios[2,3] = tbl_method_B_lead_sum$Cat_2[[1]]
lead_scenarios[2,4] = tbl_method_B_lead_sum$Cat_3B[[1]]
lead_scenarios[2,5] = tbl_method_B_lead_sum$List[[1]]

lead_scenarios[3,1] = "Set to 0"
lead_scenarios[3,2] = nrow(tbl_method_C_lead)
lead_scenarios[3,3] = tbl_method_C_lead_sum$Cat_2[[1]]
lead_scenarios[3,4] = tbl_method_C_lead_sum$Cat_3B[[1]]
lead_scenarios[3,5] = tbl_method_C_lead_sum$List[[1]]

lead_scenarios[4,1] = "Eliminate"
lead_scenarios[4,2] = nrow(tbl_method_D_lead)
lead_scenarios[4,3] = tbl_method_D_lead_sum$Cat_2[[1]]
lead_scenarios[4,4] = tbl_method_D_lead_sum$Cat_3B[[1]]
lead_scenarios[4,5] = tbl_method_D_lead_sum$List[[1]]

lead_scenarios[5,1] = "Use 1/2 Criteria or 1/2 QL"
lead_scenarios[5,2] = nrow(tbl_method_E_lead)
lead_scenarios[5,3] = tbl_method_E_lead_sum$Cat_2[[1]]
lead_scenarios[5,4] = tbl_method_E_lead_sum$Cat_3B[[1]]
lead_scenarios[5,5] = tbl_method_E_lead_sum$List[[1]]

#write.csv(lead_scenarios, "lead_scenarios.csv", row.names = FALSE)


lead_data_save <- lead_data %>%
  rename(set_det_limit = method_A,
         set_half_det_limit = method_B,
         set_0 = method_C,
         half_crit = method_E,
         set_det_limit_excur = method_A_exceed,
         set_half_det_limit_excur = method_B_exceed,
         set_0_excur = method_C_exceed,
         half_crit_excur = method_E_exceed)



Lead_QL_crit <- lead_data %>%
  mutate(QL_above_crit = ifelse(as.numeric(DetectionQuantitationLimitMeasure.MeasureValue) > as.numeric(strd), 1, 0 )) %>%
  summarise(QL_mean = mean(DetectionQuantitationLimitMeasure.MeasureValue),
            QL_min = min(DetectionQuantitationLimitMeasure.MeasureValue),
            QL_max = max(DetectionQuantitationLimitMeasure.MeasureValue),
            Crit_mean = mean(strd),
            Crit_min = min(strd),
            Crit_max = max(strd),
            n = n(),
            QL_above_crit =sum(QL_above_crit))

write.csv(Lead_QL_crit, "Lead_QL_crit_means.csv", row.names = FALSE)

#write.csv(lead_data_save, "lead_scenarios_data.csv", row.names = FALSE)

# Lead geomeans -----------------------------------------------------------

# lead_geomean <- lead_data %>%
#   ungroup() %>%
#   group_by(MonitoringLocationIdentifier, CharacteristicName) %>%
#   summarise(A_geomean = geo_mean(as.numeric(method_A)),
#             B_geomean = geo_mean(as.numeric(method_B)),
#             C_geomean = geo_mean(as.numeric(method_C)),
#             E_geomean = geo_mean(as.numeric(method_E)),
#             num_samples = n(),
#   ) %>%
#   mutate(method_A_exceed = ifelse(A_geomean > strd, 1, 0),
#          method_B_exceed = ifelse(B_geomean > strd, 1, 0),
#          method_C_exceed = ifelse(C_geomean > strd, 1, 0),
#          method_E_exceed = ifelse(E_geomean > strd, 1, 0)) 



#how do you compare to geomean when the criteria changes????

# YOu DON'T!"



# Find sites to map -------------------------------------------------------

#write.csv(method_D, "E:/Integrated Report/Nondetects/sites_to_map.csv", row.names = FALSE)
