# Copyright 2019 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


## load cleaned fedprov (all_data_sim_fedprov) and EMS
## (all_data_sim_EMS) csv files and combine into one

library(tidyverse)
library(plyr)
library(lubridate)
library(magrittr)


#Load csv's
fedprov_clean <- read_csv("data/report/all_fedprov.csv")

EMS_clean <- read_csv("data/report/similkameen_EMS.csv")


##combine the two dataframes

all_clean <- merge.data.frame(fedprov_clean, EMS_clean, all=TRUE)

## merge columns SITE_NO and EMS_ID so that the fedprov station
## names are in the EMS_ID column

all_similkameen <- transform(all_clean, EMS_ID = ifelse(is.na(EMS_ID), SITE_NO, EMS_ID))

##double check that fedprov stations show up in EMS_ID column
##params <- distinct(fedprov_EMS_sim, EMS_ID)

##remove SITE_NO  and DetectinLimit Column
all_similkameen$SITE_NO <- NULL
all_similkameen$DetectionLimit <- NULL
head(all_similkameen)

# double check that fedprov sites were merged into EMS_ID column
EMS_ID <- distinct (all_similkameen, EMS_ID)


# need to make variable names and units in the same case, choosing lower

all_similkameen <- all_similkameen %>%
  mutate (Variable = tolower(Variable))
all_similkameen <- all_similkameen %>%
  mutate (Units = tolower(Units))


# Look for the parameters we are interested in
## Do this manually by looking at the `parameters` and `all_similkameen` dataframes

parameters3 <- distinct (all_similkameen, Variable, Code, EMS_ID)

## Need to standardize names for SO4- and a few nitrogen parameters, tds

#library(plyr)
all_similkameen$Variable <- plyr::revalue (all_similkameen$Variable, c("sulphate"="sulfate dissolved", "sulphate dissolved"="sulfate dissolved", 
                                                                 "nitrogen kjel.tot(n)"="nitrogen total kjeldahl",
                                                                 "nitrogen (kjeldahl) total dissolved"="nitrogen dissolved kjeldahl",
                                                                 "colour true"="true color",  "temperature water (field)"="temperature-field",
                                                                 "residue: non-filterable (tss)"="tss", "hardness total (total)"="hardness total",
                                                                 "ammonia dissolved"="nitrogen ammonia dissolved", "temperature water"="temperature",
                                                                 "residue: filterable 1.0u (tds)"="tds", "chlorine res:free"="chlorine residual", 
                                                                 "field ph"="ph-field", "sulfate total" = "sulfate dissolved",  
                                                                 "nitrogen - nitrite dissolved (no2)"="nitrite dissolved", "nitrogen nitrite"="nitrite total",
                                                                 "nitrogen no2 total"="nitrite total", "nitrate (no3) dissolved"= "nitrate dissolved",
                                                                 "nitrogen dissolved nitrate" = "nitrate dissolved", "nitrogen total nitrate" = "nitrate dissolved",
                                                                 "nitrate(no3) + nitrite(no2) dissolved"="nitrite and nitrate dissolved",
                                                                 "nitrogen no3 total"="nitrate total", "nitrogen dissolved no3 & no2"="nitrite and nitrate dissolved",
                                                                 "total nitrogen no2 + no3"="nitrite and nitrate total", "fluoride"="fluoride total", 
                                                                 "coliform - fecal"="fecal coliform", "coliform - total"="total coliform"))
                                                               
                                                                
                                                                
                                                                 

                                                                 
                                                                  
                                                                
                                                    
                                                      
                                                     


## double check that parameter names were standardized
parameters2 <- distinct (all_similkameen, Variable, Code, Units)

## Standardize a few Units
all_similkameen$Units <- plyr::revalue (all_similkameen$Units, c("ug/g"="mg/l", "col.unit"="color unit", "cu"="color unit", "ph units"="ph",
                                               "usie/cm"="us/cm", "c"="deg c"))


write.csv(all_similkameen,
          'C:/R Projects/Similkameen-WQOs/data/report/sim_clean.csv', row.names = FALSE)
