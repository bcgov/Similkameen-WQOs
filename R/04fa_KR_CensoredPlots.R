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

## reduce dataset to parameters of interest, 
#graphing non-detects vs detects to determine if some high non-detects should be removed from dataset

### Keremeos  River sites

library(tidyverse)
library(magrittr)
library(NADA)

setwd('C:/R Projects/Similkameen-WQOs')

# Load clean data
sim_data <- read_csv("data/report/sim_final2.csv")

parm <- distinct(KR_sites, Variable)

## make dataframe of Keremeos River sites
KR_sites <- filter(sim_data, EMS_ID == "E221386" | EMS_ID == "E221384" | EMS_ID == "E221413" |
                     EMS_ID == "E221387" | EMS_ID ==  "E221390" | EMS_ID == "E221389" |
                     EMS_ID == "E221339" | EMS_ID == "E221340" | EMS_ID == "E221341" |
                     EMS_ID == "0500757")

## filter out the dates to plot, only including data from 2000 on

KR_sites <- filter(KR_sites, DateTime > "2000-01-01" & DateTime < "2019-12-31")

# Set censor for non-detect
KR_sites %<>% mutate(CENSOR = ifelse(is.na(ResultLetter) | ResultLetter == "M",
                                     FALSE, TRUE))


#make dataframe of parameters that I am interested in assessing

str(KR_sites)

#make csv for spreadsheet to be used in censored stats script
write.csv(KR_sites,'C:/R Projects/Similkameen-WQOs/data/report/KR_Plots/tables/KR_sites.csv', row.names = FALSE)


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
         "manganese total", "hardness (dissolved)", "nitrite and nitrate dissolved",
         "chloride dissolved")





## PLOT DATA 1 (in a loop, make one plot per parameter)
## First set working directory to save plots to. This section only plots with clean data. 

setwd('C:/R Projects/Similkameen-WQOs/data/report/KR_Plots/High_ND')


## plotting to look at censored data, True means <MDL. Can evaluate which method 
# to deal with consored data base on detects vs non-detects

## mg/L plots
site_mgL <- filter(KR_sites, Variable %in% mgL)



for (v in mgL) {
  mgL_plots <- filter(KR_sites, Variable == v) %>%
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


#parameters <- distinct (KR_sites, Variable, Units)

## ug/L plots

site_ugL <- filter(KR_sites, Variable %in% ugL)


for (v in ugL) {
  ugL_plots <- filter(KR_sites, Variable == v) %>%
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


site_pH <- filter(KR_sites, Variable == "ph") %>%
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

site_pH <- filter(KR_sites, Variable == "ph-field") %>%
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

site_temp <- filter(KR_sites, Variable == "temperature-field") %>%
  group_by(EMS_ID)
tempplot <- ggplot(site_temp, aes(x = EMS_ID, y = Value, color = CENSOR)) +
  geom_point() +
  ggtitle("Temperature Field") +
  xlab("EMS_ID") +
  ylab("Â°C") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(tempplot)
ggsave(filename = "temp-field.png", plot = tempplot, units= "in")


site_temp <- filter(KR_sites, Variable == "temperature") %>%
  group_by(EMS_ID)
tempplot <- ggplot(site_temp, aes(x = EMS_ID, y = Value, color = CENSOR)) +
  geom_point() +
  ggtitle("Temperature") +
  xlab("EMS_ID") +
  ylab("Â°C") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(tempplot)
ggsave(filename = "temp.png", plot = tempplot, units= "in")

## E Coli

site_ecoli <- filter(KR_sites, Variable == "e coli") %>%
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

# Enterococcus

site_ent <- filter(KR_sites, Variable == "enterococcus") %>%
  group_by(EMS_ID)
entplot <- ggplot(site_ent, aes(x = EMS_ID, y = Value, color = CENSOR)) +
  geom_point() +
  ggtitle("Enterococcus") +
  xlab("EMS_ID") +
  ylab("cfu/100mL") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(entplot)
ggsave(filename = "ent.png", plot = entplot, units= "in")

# Coliform Fecal cfu/100mL

site_col <- filter(KR_sites, Variable == "fecal coliform", Units == "cfu/100ml") %>%
  group_by(EMS_ID)
colplot <- ggplot(site_col, aes(x = EMS_ID, y = Value, color = CENSOR)) +
  geom_point() +
  ggtitle("Fecal Coliform") +
  xlab("EMS_ID") +
  ylab("cfu/100mL") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(colplot)
ggsave(filename = "fecalcoliform2.png", plot = colplot, units= "in")

## turbidity

site_tur <- filter(KR_sites, Variable == "turbidity") %>%
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

#set working drive back to main folder 
setwd('C:/R Projects/Similkameen-WQOs')

#Remove the high NDs from the KR-sites dataframe 
KR_sites %<>% filter(!(Variable == "silver total" & CENSOR == TRUE & Value >= 0.02))
                         

# Write CSV as there were some data manipulation in this script that I want to retain
# this includes subsetting the data from 2000-2010

write.csv(KR_sites,'C:/R Projects/Similkameen-WQOs/data/report/KR_Plots/tables/KR_sites2.csv', row.names = FALSE)

