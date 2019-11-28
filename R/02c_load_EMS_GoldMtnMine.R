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

# SCRIPT SUMMARY
# Download all EMS data for Similkamen WQO sites
# A file containing the raw data that is loaded and filtered by later scripts


# DOWNLOAD AND LOAD EMS DATA FROM DATA BC OPEN DATA OBJECT

# See rems readme file for more information: https://github.com/bcgov/rems/blob/master/README.Rmd

# install.packages("package name") if not already installed

#install_github("bcgov/rems")
#install_github("bcgov/wqbc", ref = "develop")

## load GoldMtn data
## use mutate to transform the date time and assign timezone (tz)
## not that str_sub is subsetting the first 10 characters of the date/time

goldmtn <- read_csv(file="data/GoldMtn.csv",
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
                                    "Temperature (In Situ)"="temperature-field", "Nitrate (as N)"="nitrogen total nitrate", "Nitrite (as N)" = "nitrogen total nitrite",
                                    "Nitrogen (Total)"="nitrogen total", "Nitrogen Kjeldahl (Total)"="nitrogen total kjeldahl",
                                    "Phosphorus (Nutrient) Dissolved"="phosphorus total dissolved", "Phosphorus (Nutrient) Total"="phosphorus total",
                                   "Hardness as CaCO3 (Dissolved)"="hardness (dissolved)",
                                    "Hardness as CaCO3 (Total)"="hardness total", "Total Dissolved Solids"="general-solidstds",
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
                                  "Mercury (Total)"="mercury total", "Cyanide (Total)"="cyanide total", "Chlorophyll-a"="chlorophyll",
                                  "Bromide (Dissolved)"="	bromide dissolved", "pH (In Situ)"= "ph-field",
                                  "Alkalinity (PP as CaCO3)"="alkalinity phenolphthalein caco3", "Ion Balance"="cation - anion balance",
                                  "Oxygen Dissolved (In Situ)"="dissolved oxygen-field", "Cyanide (SAD)"="cyanide s.a.d.",
                                  "Fluoride (Total)"="fluoride total", "Specific Conductivity"="specific conductance"))

#change dataframe to lowerwcase
goldmtn <- goldmtn %>%
  mutate (Variable = tolower(Variable),Units = tolower(Units))

## load sim_clean data frame and merge with GM
sim_clean <- read_csv("data/sim_clean.csv")

sim_gm <- merge.data.frame(sim_clean, goldmtn, all=TRUE)

##look at variables
parameters <- distinct (sim_gm, Variable)

#write csv
write.csv(sim_gm,
          'C:/R Projects/SimilkameenWQOs/data/sim_gm.csv', row.names = FALSE)

