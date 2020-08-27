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

# This script is to make a dataframe of the parameters needed to caculate the ammonia WQG
# pH, Temp, Ammonia. 
#looking for results where parameters are sampled on the same day.

library(tidyverse)

setwd("C:/R Projects/Similkameen-WQOs")

# Load clean data
sim_data <- read.csv("data/report/sim_final2.csv",  stringsAsFactors = FALSE)

# filter for timeframe 2000 to present
sim_data <- filter(sim_data, DateTime > "2000-01-01" & DateTime < "2019-12-31")

#extract parameters for ammonia guideline: nitrogen ammonia dissolved (1108), pH (0004), temperature (0013)
#note that there is no paried data for total ammonia in this dataset
NH4_dat <- sim_data %>% 
  subset(Variable %in% c("nitrogen ammonia dissolved", "ph", "temperature"))

#variables needed
vars <- c("nitrogen ammonia dissolved", "ph", "temperature")

#make a temporary data frame with a new attribute
temp <- NH4_dat %>% group_by(EMS_ID, DateTime) %>% 
  mutate(goodDataPoint = setequal(Variable, vars))

#final data frame
finalData <- temp %>% filter(goodDataPoint == "TRUE")

# take out columns that are not needed
finalData <- dplyr::select(finalData, 
                           -c(Station,goodDataPoint))

#make dataframe of NH4 and Result Letter to be later merged with guidline dataframe
ResultLetter <- dplyr::filter(finalData, Code %in% "1108")
ResultLetter <- dplyr::select(ResultLetter, -c(Variable, Code, Units))                           
colnames(ResultLetter) <- c("DateTime", "1108", "ResultLetter", "EMS_ID")                                     


#reshape to find cells with more than one result so that extra results can be removed
NH4_Data <- finalData %>%            
  pivot_wider(id_cols = c(EMS_ID, DateTime),
              names_from = c(Code), values_from = Value)
#make a column for extra result in NH4 Cell
NH4_Data <- NH4_Data %>% unnest_wider("1108") 
#rename columns
colnames(NH4_Data) <- c("EMS_ID", "DateTime", "1108", "val1", "val2", 
                       "0004", "0013")
#remove columns with extra result
NH4_Data <- dplyr::select(NH4_Data, -c(val1, val2))
#make a column for extra result in NH4 Cell
NH4_Data <- NH4_Data %>% unnest_wider("0004") 
#rename columns
colnames(NH4_Data) <- c("EMS_ID", "DateTime", "1108",  
                        "0004", "val1", "val2", "0013")
#remove columns with extra result
NH4_Data <- dplyr::select(NH4_Data, -c(val1, val2))
#make a column for extra result in NH4 Cell
NH4_Data <- NH4_Data %>% unnest_wider("0013") 
#rename columns
colnames(NH4_Data) <- c("EMS_ID", "DateTime", "1108", 
                        "0004", "0013", "val1", "val2")
#remove columns with extra result
NH4_Data <- dplyr::select(NH4_Data, -c(val1, val2))

## calculate the acute and chronic guideline for NH4
NH4_Data <- NH4_Data %>% mutate(AcuteAL = dplyr::case_when(0<= `0013` & `0013` <= 20 & 6.5 <= `0004` & `0004` <8~
                  ((0.52/(10^(0.03*(20-`0013`)))/((1+10^(7.4-`0004`))/1.25)/
               2*0.822)*100)/(1/(10^((0.09018+2729.92/(273.2+`0013`))-`0004`)+1)*100), 
0<= `0013` & `0013` <= 20 & 6.5 <= `0004` & `0004` <8~
  ((0.52/(10^(0.03*(20-`0013`)))/((1+10^(7.4-`0004`))/1.25)/
      2*0.822)*100)/(1/(10^((0.09018+2729.92/(273.2+`0013`))-`0004`)+1)*100),
0<= `0013` & `0013` <= 20 & 8 <= `0004` & `0004` <=9~
  ((0.52/(10^(0.03*(20-`0013`)))/2*0.822)*100)/(1/(10^((0.09018+2729.92/
(273.2+`0013`))-`0004`)+1)*100)

), #ending acute case_when

ChronicAL = dplyr::case_when(0<= `0013` & `0013` < 15 & 6.5 <= `0004` & `0004` < 7.7 ~
    (0.8/(10^(0.03*(20-`0013`)))/((1+10^(7.4-`0004`))/1.25)/((24*10^(7.7-`0004`))/
    (1+10^(7.4-`0004`)))*0.822*100)/(1/(10^((0.09018+2729.92/(273.2+`0013`))-`0004`)+1)*100),
    0<= `0013` & `0013` < 15 & 7.7 <= `0004` & `0004` < 8 ~
      (0.8/(10^(0.03*(20-`0013`)))/((1+10^(7.4-`0004`))/1.25)/16*0.822*100)/
      (1/(10^((0.09018+2729.92/(273.2+`0013`))-`0004`)+1)*100),
    
    0 <= `0013` & `0013` < 15 & 8 <= `0004` & `0004` <= 9 ~
      (0.8/(10^(0.03*(20-`0013`)))/16*0.822*100)/(1/(10^((0.09018+2729.92/(273.2+`0013`))-`0004`)+1)*100),
    
    15<= `0013` & `0013` < 20 & 6.5 <= `0004` & `0004` < 7.7 ~
      (0.8/1.14/((1+10^(7.4-`0004`))/1.25)/((24*10^(7.7-`0004`))/(1+10^(7.4-`0004`)))*0.822*100)/
      (1/(10^((0.09018+2729.92/(273.2+`0013`))-`0004`)+1)*100),
    
    15<= `0013` & `0013` < 20 & 7.7 <= `0004` & `0004` < 8 ~
      (0.8/1.14/((1+10^(7.4-`0004`))/1.25)/16*0.822*100)/
      (1/(10^((0.09018+2729.92/(273.2+`0013`))-`0004`)+1)*100),
    
    15<= `0013` & `0013` <20 & 8 <= `0004` & `0004` <= 9 ~
      (0.8/1.14/16*0.822*100)/(1/(10^((0.09018+2729.92/(273.2+`0013`))-`0004`)+1)*100)  
  
)#ending chronic case when
)#ending mutate 

#make dataframe of Chronic guideline to remove NAs
Chronic<- dplyr::select(NH4_Data, -AcuteAL)
#remove rows with NA
Chronic <- na.omit(Chronic) 
#make dataframe of Acute guideline to remove NAs
Acute <- dplyr::select(NH4_Data, -ChronicAL)
#remove rows with NA
Acute <- na.omit(Acute)

#join chronic and acute guideline tables
WQG <- dplyr::full_join(Chronic, Acute, by = c("EMS_ID", "DateTime", "1108", "0004", "0013"))

#remove ph and temperature columns
WQG <- dplyr::select(WQG, -c(`0004`, `0013`))

# join WQG with Result Letter 
NH4WQG <- dplyr::full_join(WQG, ResultLetter, by = c("EMS_ID", "DateTime", "1108"))

#write csv of wide format  
write.csv(NH4WQG,'C:/R Projects/Similkameen-WQOs/data/report/NH4_guideline_wide.csv', row.names = FALSE)  

#make table in long format
NH4_long <- NH4WQG %>% 
  pivot_longer(-c(EMS_ID, DateTime, ResultLetter), names_to = "Code", values_to = "Value")

# Set censor for non-detect
NH4_long <- NH4_long %>% mutate(CENSOR = ifelse(is.na(ResultLetter) | ResultLetter == "M",
                                              FALSE, TRUE)) 

#change case of censor column
NH4_long$CENSOR <- gsub("FALSE", "False", NH4_long$CENSOR)
NH4_long$CENSOR <- gsub("TRUE", "True", NH4_long$CENSOR)

#rename code column to variable
colnames(NH4_long) <- c("EMS_ID", "DateTime", "ResultLetter", 
                        "Variable", "Value", "CENSOR")

#rename 1108 to dissolved ammonia
NH4_long$Variable <- plyr::revalue (NH4_long$Variable,
                                   c(`1108`="dissolved ammonia"))


#write csv of long format  
write.csv(NH4_long,'C:/R Projects/Similkameen-WQOs/data/report/NH4_guidelines_long.csv', row.names = FALSE)  




















































































