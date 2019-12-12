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

## Load packages from script 01_load_EMS.R if needed

## load CABIN data only the columns that I want, and assign vector (character, factor)
## change column headings  in CABIN dataframe to match Similkameen dataframe
## use mutate to transform the date time and assign timezone (tz)
## not that str_sub is subsetting the first 10 characters of the date/time

CABIN1 <- read_csv(file="data/CABIN_hor.csv",
                   col_types = cols_only(Site=col_character(),
                                         SampleDate=col_character(),
                                        Name=col_character(),
                                        SampleStatus=col_character(),
                                        Variable=col_factor(),
                                        Value=col_double(),
                                        Unit=col_factor() )) %>%
  dplyr::rename(EMS_ID=Site, DateTime=SampleDate, Station=Name, Units=Unit) %>%
  mutate(DateTime=str_sub(DateTime, 1,10),
         DateTime=as.POSIXct(DateTime, format="%Y-%m-%d", tz="UTC"))


## to turn dataframe into tibble, tibble will show me what format the columns are in
## CABIN1 <- as_tibble(CABIN1)

## Rename variables in CABIN to match Similkameen
CABIN1$Variable <- plyr::revalue (CABIN1$Variable,
      c("Ag"="silver total",
        "Al"="aluminum total", "As"="arsenic total",
        "B"="boron total", "Ba"="barium total",
        "Be"="beryllium total", "Bi"="bismuth total",
        "Br"="bromine total", "Ca"="calcium total",
        "Cd"="cadmium total", "Chloride-Dissolved"="chloride dissolved",
        "Co"="cobalt total", "Cr"="chromium total",
        "Cu"="copper total", "F"="fluoride total",
        "Fe"="iron total", "General-Alkalinity"="alkalinity:total",
        "General-CarbonDIC"="carbon dissolved inorganic",
        "General-CarbonDOC"="carbon dissolved organic",
        "General-CarbonTOC"="carbon total organic",
        "General-DO"="oxygen dissolved",
        "General-Hardness"="hardness total",
        "General-pH"="pH", "General-SolidsTSS"="tss",
        "General-SpCond"= "specific conductance",
        "General-TempWater"="temperature water",
        "General-Turbidity"="turbidity", "K"="potassium total",
        "Li"="lithium total", "Mg"="magnesium total",
        "Mn"="manganese total", "Mo"="molybdenum total",
        "Na"="sodium total", "Ni"="nickel total",
        "Nitrogen-NH3"="nitrogen ammonia total",
        "Nitrogen-NO2"="nitrogen total nitrite",
        "Nitrogen-NO2+NO3"="nitrogen total no3 & no2",
        "Nitrogen-NO3"="nitrogen total nitrate",
        "Nitrogen-TKN"="nitrogen total kjeldahl",
        "Nitrogen-TN"="nitrogen total",
        "Nitrogen-TN_Organic"="nitrogen organic-total",
        "Pb"="lead total", "Phosphorus-OrthoP"="phosphorus ortho",
        "Phosphorus-TDP"="phosphorus total dissolved",
        "Phosphorus-TP"="phosphorus total", "S"="sulfur total",
        "Sb"="antimony total", "Se"= "selenium total",
        "Si"="silicon total", "Sn"="tin total",
        "SO4"="sulfate total", "Sr"="strontium total",
        "Ti"="titanium total", "Tl"="thallium total",
        "U"="uranium total", "V"="vanadium total",
        "Zn"="zinc total", "Zr"="zirconium total"))

## double check that parameter names standardized and what units are in the dataframe
parameters1<-distinct(CABIN1, Units, Variable)


# remove rows: General-Conductivity, General-TempAir
## decode to change to the standardized units in the similkameen dataframe
CABIN2 <- CABIN1 %>% dplyr::filter(!Variable=="General-Conductivity",
                                   !Variable=="General-TempAir")


## change units for temperature from Degrees Celsius to deg c

CABIN2$Units <- revalue (CABIN2$Units, c("Degrees Celsius"="deg c"))

## make all units and cariables lowercase to match similkameen dataframe

CABIN2 <- CABIN2 %>%
  mutate (Variable = tolower(Variable),Units = tolower(Units))

## load sim_clean data frame and merge with CABIN2
sim_clean <- read_csv("data/sim_clean.csv")

simCABIN <- merge.data.frame(sim_clean, CABIN2, all=TRUE)

# CREATE CSV OF RAW DATA
write.csv(simCABIN,'C:/R Projects/Similkameen-WQOs/data/report/simCABIN.csv', row.names = FALSE)
