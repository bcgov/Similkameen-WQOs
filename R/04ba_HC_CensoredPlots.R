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

## reduce dataset to parameters that I am interested in, 
#graphing non-detects vs detects to determine if some high non-detects should be removed from dataset

### Similkameen River sites

library(tidyverse)
library(magrittr)
library(NADA)

setwd('C:/R Projects/Similkameen-WQOs')

# Load clean data
sim_data <- read_csv("data/report/sim_final2.csv")

##HC_Sites: "E223876", "E223873", "E223874"

## make dataframe of Similkameen River sites
HC_sites <- filter(sim_data, EMS_ID == "E223873" | EMS_ID == "E223874") 

# Set censor for non-detect
HC_sites %<>% mutate(CENSOR = ifelse(is.na(ResultLetter) | ResultLetter == "M",
                                     FALSE, TRUE))


#change variable and EMS_ID to character
HC_sites$Variable <- as.character(HC_sites$Variable)
HC_sites$EMS_ID <- as.character(HC_sites$EMS_ID)

#make csv for spreadsheet to be used in censored stats script
write.csv(HC_sites,'C:/R Projects/Similkameen-WQOs/data/report/HC_Plots/tables/HC_Sites.csv', row.names = FALSE)

## Set a vector of the parameters we are interested in
## Do this manually by looking at the `parameters` and `sim_clean` dataframes

ugL <- c("cyanide wad", "arsenic total", "cadmium total",  "cadmium dissolved",  "chromium total",  
         "chromium dissolved", "arsenic dissolved",  "copper total",  "copper dissolved",  
         "lead total", "lead dissolved", "mercury total", "mercury dissolved", 
         "nickel total", "nickel dissolved", "uranium total", "uranium dissolved",
         "zinc total", "zinc dissolved", "silver total", "silver dissolved", "selenium total",
         "selenium dissolved", "cobalt total", "cobalt dissolved", "cyanide s.a.d.")


mgL <- c("alkalinity total 4.5", "alkalinity total caco3", "tss", "tds",  "dissolved oxygen-field",
         "nitrogen ammonia dissolved", "nitrogen ammonia total", "nitrate total", "nitrate dissolved",
         "nitrite total", "oxygen dissolved", "phosphorus total",  "phosphorus total dissolved",
         "nitrite dissolved", "sulfate dissolved",  "carbon dissolved organic", "calcium dissolved",
         "carbon total organic",  "hardcalc", "magnesium dissolved", "hardness total", 
         "nitrogen total", "manganese dissolved", "molybdenum total", "molybdenum dissolved", 
         "iron total",  "iron dissolved", "aluminum total", "aluminum dissolved",
         "manganese total", "hardness (dissolved)")
          
         




## PLOT DATA 1 (in a loop, make one plot per parameter)
## First set working directory to save plots to. This section only plots with clean data. 

setwd('C:/R Projects/Similkameen-WQOs/data/report/HC_Plots/High_ND')


## plotting to look at censored data, True means <MDL. Can evaluate which method 
# to deal with consored data base on detects vs non-detects. Can plot all in mg/L.

## mg/L plots
site_mgL <- filter(HC_sites, Variable %in% mgL)



for (v in mgL) {
  mgL_plots <- filter(HC_sites, Variable == v) %>%
    group_by(EMS_ID)
  plot <- ggplot(mgL_plots, aes(x = EMS_ID, y = Value,
                                colour = CENSOR)) +
    geom_point() +
    ggtitle(v) +
    
    xlab("EMS_ID") +
    ylab("mg/L") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  plot(plot)
  ggsave(filename = paste0(v,".png"), plot = plot, units= "in")
  
}

## ug/L plots

site_ugL <- filter(HC_sites, Variable %in% ugL)


for (v in ugL) {
  ugL_plots <- filter(HC_sites, Variable == v) %>%
    group_by(EMS_ID)
  plot <- ggplot(ugL_plots, aes(x = EMS_ID, y = Value,
                                colour = CENSOR)) +
    geom_point() +
    ggtitle(v) +
    
    xlab("EMS_ID") +
    ylab("ug/L") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  plot(plot)
  ggsave(filename = paste0(v,".png"), plot = plot, units= "in")
  
}
## Individual graphs for pH, turbidity, temp, e coli cfu/100ml (no fecal coliform)

#pH


site_pH <- filter(HC_sites, Variable == "ph") %>%
  group_by(EMS_ID)
pHplot <- ggplot(site_pH, aes(x = EMS_ID, y = Value, color = CENSOR)) +
  geom_point() + 
  ggtitle("pH") +
  xlab("EMS_ID") +
  ylab("pH") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(pHplot)
ggsave(filename = "pH.png", plot = pHplot, units= "in")

#ph field

site_pH <- filter(HC_sites, Variable == "ph-field") %>%
  group_by(EMS_ID)
pHplot <- ggplot(site_pH, aes(x = EMS_ID, y = Value, color = CENSOR)) +
  geom_point() + 
  ggtitle("pH") +
  xlab("EMS_ID") +
  ylab("pH") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(pHplot)
ggsave(filename = "pH field.png", plot = pHplot, units= "in")

## Temperature

site_temp <- filter(HC_sites, Variable == "temperature-field") %>%
  group_by(EMS_ID)
tempplot <- ggplot(site_temp, aes(x = EMS_ID, y = Value, color = CENSOR)) +
  geom_point() +
  ggtitle("Temperature Field") +
  xlab("EMS_ID") +
  ylab("°C") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(tempplot)
ggsave(filename = "temp-field.png", plot = tempplot, units= "in")


## E Coli

site_ecoli <- filter(HC_sites, Variable == "e coli") %>%
  group_by(EMS_ID)
ecoliplot <- ggplot(site_ecoli, aes(x = EMS_ID, y = Value, color = CENSOR)) +
  geom_point() +
  ggtitle("E. Coli.") +
  xlab("EMS_ID") +
  ylab("cfu/100mL") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(ecoliplot)
ggsave(filename = "ecoli.png", plot = ecoliplot, units= "in")

## turbidity

site_tur <- filter(HC_sites, Variable == "turbidity") %>%
  group_by(EMS_ID)
turplot <- ggplot(site_tur, aes(x = EMS_ID, y = Value, color = CENSOR)) +
  geom_point()  + 
  ggtitle("Turbidity") +
  xlab("Date") +
  ylab("NTU") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(turplot)
ggsave(filename = "turbidity.png", plot = turplot, units= "in")

# Reviewed plots and dataframe, taking outhigh non-detects for certain parameters
# to reduce overestimation of concentrations, make a dataframe of rows I want to remove


HC_sites %<>% filter(!(Variable == "arsenic total" & CENSOR == TRUE & Value >= 60| 
                         Variable == "arsenic dissolved" & CENSOR == TRUE & Value >= 5|
                         Variable == "molybdenum total" & CENSOR == TRUE & Value >= 0.01|
                         Variable == "molybdenum dissolved" & CENSOR == TRUE & Value >= 0.03|
                         Variable == "lead total" & CENSOR == TRUE & Value >= 60|
                         Variable == "lead dissloved" & CENSOR == TRUE & Value >= 50| 
                         Variable == "chromium dissloved" & CENSOR == TRUE & Value >= 10|
                         Variable == "selenium total" & CENSOR == TRUE & Value >= 60| 
                         Variable == "selenium dissolved" & CENSOR == TRUE & Value >= 200))

# Write CSV as there were some data manipulation in this script that I want to retain
# this includes subsetting the data from 2000-2010

write.csv(HC_sites,'C:/R Projects/Similkameen-WQOs/data/report/HC_Plots/tables/HC_Sites2.csv', row.names = FALSE)
