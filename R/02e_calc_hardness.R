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

#This script calculates hardness from Ca and Mg with the following equation: (2.497XCa)+(4.118XMg)

#Load packages
library(tidyverse)
library(magrittr)

# Load clean data
sim_data <- read.csv("data/report/sim_final.csv",  stringsAsFactors = FALSE)

# filter for timeframe 2000 to present
sim_data <- filter(sim_data, DateTime > "2000-01-01" & DateTime < "2019-12-31")

#remove station 0500073 as it is fedprov station BC08NL0001
sim_data <- subset(sim_data, EMS_ID!="0500073")

#calculate hardness from Ca and Mg and make a new column
# first make dataframe of calcium and magnesium
Ca <- filter(sim_data, Code %in% "CA-D")
Mg <- filter(sim_data, Code %in% "MG-D")
#multiply Ca by 2.97
Ca$Value <- Ca$Value * 2.97
#multiply Mg by 4.118
Mg$Value <- Mg$Value *4.118
#combine Ca&Mg tables
CaMg <- dplyr::union(Ca, Mg)

#remove some columns
CaMg <- dplyr::select(CaMg, -c(Code, Units, ResultLetter, Station)) 
                       
#for each date combination of Ca and Mg, calculate hardness
#reshape data so Ca and Mg are in columns for each date
CaMg <- CaMg %>%
  pivot_wider(id_cols = c(EMS_ID, DateTime),
              names_from = Variable, values_from = Value) 

#make a column for extra result in CA-D ell 
CaMg <- CaMg %>% unnest_wider("calcium dissolved")
#rename columns
colnames(CaMg) <- c("EMS_ID", "DateTime", "calcium_dissolved", 
                    "cal2", "cal3", "magnesium_dissolved")
#remove column with extra result
CaMg <- dplyr::select(CaMg, -c(cal2, cal3))
#make a column for extra result in MG-D ell 
CaMg <- CaMg %>% unnest_wider("magnesium_dissolved")
#rename columns
colnames(CaMg) <- c("EMS_ID", "DateTime", "calcium_dissolved", 
                   "magnesium_dissolved", "mag2", "mag3")
#remove column with extra result
CaMg <- dplyr::select(CaMg, -c(mag2, mag3))

#adding Ca and Mg columns and create hardess column, first change columns to numeric
CaMg$calcium_dissolved <- as.numeric(CaMg$calcium_dissolved)                     
CaMg$magnesium_dissolved <- as.numeric(CaMg$magnesium_dissolved) 

CaMg <- CaMg %>% dplyr::mutate(Value = 
                                 calcium_dissolved + magnesium_dissolved)  

#look for missing data
missing <- CaMg %>% filter_all(any_vars((is.na(.))))
#remove rows with NA
CaMg <- na.omit(CaMg) 
#remove Ca and Mg columns
CaMg <- subset(CaMg, select = -c(calcium_dissolved, magnesium_dissolved))
#add columns important for rejoin to plotdata
CaMg[c("Variable", "Code", "Units")] <- NA
CaMg$Variable[is.na(CaMg$Variable)] <- "hardcalc"
CaMg$Code[is.na(CaMg$Code)] <- "H--C"
CaMg$Units[is.na(CaMg$Units)] <- "mg/l"


#join table to plotdata
sim_final <- bind_rows(sim_data, CaMg, id=NULL)

# CREATE CSV OF RAW DATA
write.csv(sim_final,'C:/R Projects/Similkameen-WQOs/data/report/sim_final2.csv', row.names = FALSE)

