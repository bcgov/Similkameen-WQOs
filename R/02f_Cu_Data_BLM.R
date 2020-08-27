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

# This script is to make a dataframe of the four parameters (d Cu, DOC, pH, d hard) needed to run the Cu BLM model. looking for results
# where all four parameters are sampled on the same day.

library(dplyr)
library(tidyr)

# Load clean data
sim_data <- read.csv("data/report/sim_final2.csv",  stringsAsFactors = FALSE)

# extract the four parameters from the main dataframe
Cu_dat <- sim_data %>% 
  subset(Variable %in% c("copper dissolved", "ph", "carbon dissolved organic",
                         "hardcalc"))

# filter for timeframe 2000 to present
Cu_dat <- filter(Cu_dat, DateTime > "2000-01-01" & DateTime < "2019-12-31")

#variables needed
vars <- c("copper dissolved", "ph", "carbon dissolved organic", 
          "hardcalc")

#make a temporary data frame with a new attribute
temp <- Cu_dat %>% group_by(EMS_ID, DateTime) %>% 
  mutate(goodDataPoint = setequal(Variable, vars))

#final data frame
finalData <- temp %>% filter(goodDataPoint == "TRUE")

# take out columns that are not needed
finalData <- dplyr::select(finalData, 
  -c(Station,  goodDataPoint))

#make dataframe of NH4 and Result Letter to be later merged with guidline dataframe
ResultLetter <- dplyr::filter(finalData, Variable %in% "copper dissolved")
ResultLetter <- dplyr::select(ResultLetter, -c(Variable, Code, Units))                           
colnames(ResultLetter) <- c("DateTime", "copperdissolved", "ResultLetter", "EMS_ID")                                     

#reshape into format for BLM analysis 
CuData <- finalData %>%            
  pivot_wider(id_cols = c(EMS_ID, DateTime),
              names_from = Variable, values_from = Value)

#make a column for extra result in CU-D cell 
CuData <- CuData %>% unnest_wider("copper dissolved")
#rename columns
colnames(CuData) <- c("EMS_ID", "DateTime", "carbon dissolved organic", "ph",
                    "copperdissolved", "val", "hardcalc")
#remove column with extra result
CuData <- dplyr::select(CuData, -c(val))
#make a column for extra result in ph cell 
CuData <- CuData %>% unnest_wider("ph")
#rename columns
colnames(CuData) <- c("EMS_ID", "DateTime", "carbon dissolved organic", "ph", "val", 
                      "copperdissolved", "hardcalc")
#remove column with extra result
CuData <- dplyr::select(CuData, -c(val))
#make a column for extra result in DOC cell 
CuData <- CuData %>% unnest_wider("carbon dissolved organic")
#rename columns
colnames(CuData) <- c("EMS_ID", "DateTime", "carbon dissolved organic", "val", "ph",
                      "copperdissolved", "hardcalc")
#remove column with extra result
CuData <- dplyr::select(CuData, -c(val))
#make a column for extra result in hardcalc cell 
CuData <- CuData %>% unnest_wider("hardcalc")
#rename columns
colnames(CuData) <- c("EMS_ID", "DateTime", "carbon dissolved organic", "ph", 
                      "copperdissolved", "hardcalc")

#write csv to run in Cu BLM model
write.csv(CuData,'C:/R Projects/Similkameen-WQOs/data/report/CuFinalBLM.csv', row.names = FALSE)

#load up csv with WQGs from BLM to put back into long format
CuWQGs <- read.csv("data/report/CuFinalBLM3.csv",  stringsAsFactors = FALSE)

#remove columns from dataset in preparation to merge with ResultLetter
CuWQGs2 <- dplyr::select(CuWQGs, -c(Site.name, Temp, ph, DOC, hardcalc))

# join WQG with Result Letter 
CuWQGs2 <- dplyr::full_join(CuWQGs2, ResultLetter, by = c("EMS_ID", "DateTime",
                                                          "copperdissolved"))

# Set censor for non-detect
CuWQGs2 <- CuWQGs2 %>% mutate(CENSOR = ifelse(is.na(ResultLetter) | ResultLetter == "M",
                                              FALSE, TRUE))   

CuWQGs3 <- CuWQGs2 %>% 
  pivot_longer(-c(EMS_ID, DateTime, ResultLetter, CENSOR), names_to = "Variable", values_to = "Value")

#change case of censor column
CuWQGs3$CENSOR <- gsub("FALSE", "False", CuWQGs3$CENSOR)
CuWQGs3$CENSOR <- gsub("TRUE", "True", CuWQGs3$CENSOR)

#write csv for long format with WQGs
write.csv(CuWQGs3,'C:/R Projects/Similkameen-WQOs/data/report/CuWQGs_long.csv', row.names = FALSE)
