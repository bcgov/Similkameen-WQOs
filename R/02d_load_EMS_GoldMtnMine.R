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

library(tidyverse)
library(plyr)
library(lubridate)
library(magrittr)

# SCRIPT SUMMARY
## load GoldMtn data
## use mutate to transform the date time and assign timezone (tz)
## not that str_sub is subsetting the first 10 characters of the date/time

goldmtn <- read_csv(file="data/report/GoldMtn.csv",
                   col_types = cols_only(EMS_ID=col_character(),
                                         DateTime=col_character(),
                                          ResultLetter=col_character(),
                                         Variable=col_factor(),
                                         Value=col_double(),
                                         Units=col_factor() )) %>%
  mutate(DateTime=str_sub(DateTime, 1,10),
         DateTime=as.POSIXct(DateTime, format="%Y-%m-%d", tz="UTC"))


## Look at parameters and units
parameters <- distinct (goldmtn, Variable)
units <- distinct (goldmtn, Units)
EMS <- distinct(goldmtn, EMS_ID)
## Rename variables in CABIN to match Similkameen
goldmtn$Variable <- plyr::revalue (goldmtn$Variable,
                                  c("Chloride (Dissolved)"="chloride dissolved", "Sulphate (Dissolved)"="sulfate dissolved",
                                    "Aluminum (Dissolved)"="aluminum dissolved", "Antimony (Dissolved)"="antimony dissolved",
                                    "Arsenic (Dissolved)"="arsenic dissolved", "Barium (Dissolved)"="barium dissolved",
                                    "Beryllium (Dissolved)"="beryllium dissolved", "Bismuth (Dissolved)"="	bismuth dissolved",
                                    "Boron (Dissolved)"="boron dissolved", "Cadmium (Dissolved)"="cadmium dissolved",
                                    "Calcium (Dissolved)"="calcium dissolved", "Chromium (Dissolved)"="chromium dissolved",
                                    "Cobalt (Dissolved)"="cobalt dissolved", "Copper (Dissolved)"="copper dissolved",
                                    "Iron (Dissolved)"= "iron dissolved", "Lead (Dissolved)"="lead dissolved",
                                    "Lithium (Dissolved)"="lithium dissolved", "Magnesium (Dissolved)"="magnesium dissolved",
                                    "Manganese (Dissolved)"="manganese dissolved", "Molybdenum (Dissolved)"="molybdenum dissolved",
                                    "Nickel (Dissolved)"="nickel dissolved", "Phosphorus (Metal) Dissolved"="phosphorus total dissolved",
                                    "Potassium (Dissolved)"="potassium dissolved", "Selenium (Dissolved)"="	selenium dissolved",
                                    "Silicon (Dissolved)"="silicon dissolved", "Silver (Dissolved)"="silver dissolved",
                                    "Sodium (Dissolved)"="sodium dissolved", "Strontium (Dissolved)"="strontium dissolved",
                                    "Sulphur (Dissolved)"="sulfur dissolved", "Tellurium (Dissolved)"="tellerium dissolved",
                                    "Thallium (Dissolved)"="	thallium dissolved", "Tin (Dissolved)"="	tin dissolved",
                                    "Titanium (Dissolved)"="	titanium dissolved", "Uranium (Dissolved)"="uranium dissolved",
                                    "Vanadium (Dissolved)"="vanadium dissolved", "Zinc (Dissolved)"="zinc dissolved",
                                    "Zirconium (Dissolved)"="	zirconium dissolved", "Specific Conductivity (In Situ)"="specific conductivity-field",
                                    "Temperature (In Situ)"="temperature-field", "Nitrate (as N)"="nitrate total", "Nitrite (as N)" = "nitrite total",
                                    "Nitrogen (Total)"="nitrogen total", "Nitrogen Kjeldahl (Total)"="nitrogen total kjeldahl",
                                    "Phosphorus (Nutrient) Dissolved"="phosphorus total dissolved", "Phosphorus (Nutrient) Total"="phosphorus total",
                                   "Hardness as CaCO3 (Dissolved)"="hardness (dissolved)",
                                    "Hardness as CaCO3 (Total)"="hardness total", "Total Dissolved Solids"="tds",
                                    "Total Suspended Solids"="tss", "Aluminum (Total)"="aluminum total", "Antimony (Total)"="antimony total",
                                    "Arsenic (Total)"="arsenic total", "Barium (Total)"="barium total",
                                    "Beryllium (Total)"="beryllium total", "Bismuth (Total)"="bismuth total",
                                    "Boron (Total)"="boron total", "Cadmium (Total)"="cadmium total",
                                    "Calcium (Total)"="calcium total", "Chromium (Total)"="chromium total",
                                    "Cobalt (Total)"="cobalt total", "Copper (Total)"="copper total",
                                    "Iron (Total)"="iron total", "Lead (Total)"="	lead total",
                                    "Lithium (Total)"="lithium total", "Magnesium (Total)"="magnesium total",
                                    "Manganese (Total)"="manganese total", "Molybdenum (Total)"="molybdenum total",
                                    "Nickel (Total)"="nickel total", "Phosphorus (Metal) Total"="phosphorus total",
                                    "Potassium (Total)"="potassium total", "Selenium (Total)"="selenium total",
                                    "Silicon (Total)"="silicon total", "Silver (Total)"="silver total", "Sodium (Total)"="sodium total",
                                    "Strontium (Total)"="strontium total", "Tin (Total)"="tin total",
                                  "Sulphur (Total)"="sulfur total", "Tellurium (Total)"="tellurium total",
                                  "Thallium (Total)"="thallium total", "Titanium (Total)"="titanium total",
                                  "Uranium (Total)"="uranium total", "Vanadium (Total)"="vanadium total",
                                  "Zinc (Total)"="zinc total", "Zirconium (Total)"="zirconium total",
                                  "pH pH"="pH", "Turbidity NTU"="turbidity", "Tungsten (Dissolved)"="tungsten dissolved",
                                  "Tungsten (Total)"="tungsten total", "Cyanide (WAD)"="cyanide wad",
                                  "Fluoride (Dissolved)"="fluoride dissolved", "Mercury (Dissolved)"="mercury dissolved",
                                  "Ammonia (Total)"="nitrogen ammonia total", "Carbon Organic (Dissolved)"="carbon dissolved organic",
                                  "Carbon Organic (Total)"="carbon total organic", "Alkalinity (Total as CaCO3)"="alkalinity total caco3",
                                  "Mercury (Total)"="mercury total", "Cyanide (Total)"="cyanide total", "Chlorophyll-a"="chlorophyll a",
                                  "Bromide (Dissolved)"="	bromide dissolved", "pH (In Situ)"= "ph-field",
                                  "Alkalinity (PP as CaCO3)"="alkalinity phenolphthalein caco3", "Ion Balance"="cation - anion balance",
                                  "Oxygen Dissolved (In Situ)"="dissolved oxygen-field", "Cyanide (SAD)"="cyanide s.a.d.",
                                  "Fluoride (Total)"="fluoride total", "Specific Conductivity"="specific conductance"))

#change dataframe to lowerwcase
goldmtn <- goldmtn %>%
  mutate (Variable = tolower(Variable),Units = tolower(Units))

#some variables have results in both mg/L and ug/L, standardize to mg/L, sim_clean data is also in mg/L
goldmtn <- goldmtn %>% dplyr::mutate(Value=ifelse(Units == "ug/l", Value/1000, Value)) %>%
  mutate(Units = ifelse(Units == "ug/l", "mg/l", Units))

## load sim_clean data frame and merge with GM
sim_clean <- read_csv("data/report/sim_recode.csv")


sim_all <- merge.data.frame(sim_clean, goldmtn, all=TRUE)

#subset variables of interest that have standardized coding from previous script
sim_all <- sim_all %>% select(DateTime, Variable, Code, Value, Units, ResultLetter, EMS_ID, Station) %>%
  filter(Variable %in% c("cyanide wad", "arsenic total", "cadmium total",  "cadmium dissolved",  "chromium total",  
                         "chromium dissolved", "arsenic dissolved", 
                         "copper total",  "copper dissolved", "nitrite and nitrate dissolved",
                         "lead total", "lead dissolved", "mercury total", "mercury dissolved", 
                         "nickel total", "nickel dissolved", "uranium total", "uranium dissolved",
                         "zinc total", "zinc dissolved", "silver total", "silver dissolved", "selenium total",
                         "selenium dissolved", "cobalt total", "cobalt dissolved", "cyanide s.a.d.",
                         "alkalinity total 4.5", "alkalinity total caco3", "tss", "tds", "dissolved oxygen-field",
                         "nitrogen ammonia dissolved", "nitrogen ammonia total", "nitrate total", "nitrate dissolved", "nitrite total",
                         "oxygen dissolved", "phosphorus total",  "phosphorus total dissolved", "nitrite dissolved",
                         "sulfate dissolved",  "carbon dissolved organic", "magnesium dissolved", "hardness total",
                         "nitrogen total", "ph", "ph-field", "temperature-field", "temperature", 
                         "calcium dissolved", "carbon total organic", "chloride total", "chloride dissolved",
                         "e coli", "enterococcus", "fecal coliform", "turbidity", "iron total",  "iron dissolved",
                         "aluminum total", "aluminum dissolved", "manganese total", "manganese dissolved", "molybdenum total", 
                         "molybdenum dissolved", "specific conductance",  "hardness (dissolved)", "specific conductivity-field",
                         "dissolved oxygen-field")) 

##make table of EMS codes
parameters <- distinct (sim_all, Variable, Code)

# use parameters to match codes from EMS data in sim_all with Gold Mountain Data

sim_all$Code <- parameters$Code[match(sim_all$Variable, parameters$Variable)]

## separate parameters by units, everything is in mg/L, need to convert some parameters back
## to ug/L
sim_all1 <- sim_all %>% 
  subset(Variable %in% 
           c("alkalinity total 4.5", "alkalinity total caco3", "tss", "tds",  "dissolved oxygen-field",
             "nitrogen ammonia dissolved", "nitrogen ammonia total", "nitrate total", "nitrate dissolved",
             "nitrite total", "oxygen dissolved", "phosphorus total",  "phosphorus total dissolved",
             "nitrite dissolved", "sulfate dissolved",  "carbon dissolved organic", "calcium dissolved",
             "carbon total organic", "magnesium dissolved", "hardness total", "nitrite and nitrate dissolved",
             "nitrogen total", "ph", "ph-field", "temperature-field", "temperature", "e coli", "enterococcus", 
             "turbidity", "iron total",  "iron dissolved", "aluminum total", "aluminum dissolved",
             "manganese total", "manganese dissolved", "molybdenum total", "molybdenum dissolved",
             "specific conductance", "hardness (dissolved)", "fecal coliform",
             "specific conductivity-field", "dissolved oxygen-field", "chloride total", "chloride dissolved")) 


sim_all2 <- sim_all %>% subset(Variable %in%  c("cyanide wad", "arsenic total", "cadmium total",  "cadmium dissolved",  "chromium total",  
                          "chromium dissolved", "arsenic dissolved",  "copper total",  "copper dissolved",  
                          "lead total", "lead dissolved", "mercury total", "mercury dissolved", 
                          "nickel total", "nickel dissolved", "uranium total", "uranium dissolved",
                          "zinc total", "zinc dissolved", "silver total", "silver dissolved", "selenium total",
                          "selenium dissolved", "cobalt total", "cobalt dissolved", "cyanide s.a.d."))


sim_all2 <- sim_all2 %>% dplyr::mutate(Value=ifelse(Units == "mg/l", Value*1000, Value)) %>%
  mutate(Units = ifelse(Units == "mg/l", "ug/l", Units))

#rejoin tables
sim_final <- dplyr::bind_rows(sim_all1, sim_all2, id=NULL) 


#write csv
write.csv(sim_final,
          'C:/R Projects/Similkameen-WQOs/data/report/sim_final.csv', row.names = FALSE)

