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

## Load packages from script 01_load.R if needed

## preparing Similkameen dataset to be compared to WQOs and WQGs
## need to standardize some EMS codes

# Load clean data
sim_clean <- read_csv("data/report/sim_clean.csv") %>%
  
  
  
  ##Standardize Date and Time
  mutate(DateTime=str_sub(DateTime, 1,10),
         DateTime=as.POSIXct(DateTime, format="%Y-%m-%d", tz="UTC"))

parameters <- distinct (sim_clean, Variable, Units, Code)

## Neeed to standardize EMS/EC codes

## this package reads excel files
library(readxl)

#load up codes worksheet, set columns to character and name
Codes <- read_excel(file.choose(), 
                    sheet="ENV_EC_Codes", col_types = "text", col_names = 
                      c("Variable", "EMS", "EMS1", "EC1", "EC2", "EC3"),
                    skip=1)

## check that R read in Codes worksheet columns as characters
str(Codes)

#going to filter out variables that need the code standardized to EMS


sim_recode <- sim_clean %>% 
  dplyr::group_by(Variable) %>%
  dplyr::mutate(Code = ifelse(Code %in% Codes$EMS1, 
                              Codes %>% filter(EMS1 %in% Code) %>% pull(EMS),
                              Code), 
                Code = ifelse(Code %in% Codes$EC1, 
                              Codes %>% filter(EC1 %in% Code) %>% pull(EMS),
                              Code),
                Code = ifelse(Code %in% Codes$EC2, 
                              Codes %>% filter(EC2 %in% Code) %>% pull(EMS),
                              Code),
                Code = ifelse(Code %in% Codes$EC3, 
                              Codes %>% filter(EC3 %in% Code) %>% pull(EMS),
                              Code)
  ) 

##look to see that recoding worked
parameters <- distinct (sim_recode, Variable, EMS_ID, Code, Value)              


## there is one instance where the EMS code is also a EC code, manully changing to the correct code
sim_recode <- sim_recode %>% mutate(Code=ifelse(Code==1103 & Variable=="fluoride dissolved",1106,Code)) 

 ## take this on end if you want to view what was done  
## %>% View(.)

write.csv(sim_recode,
          'C:/R Projects/Similkameen-WQOs/data/report/sim_recode.csv', row.names = FALSE)


