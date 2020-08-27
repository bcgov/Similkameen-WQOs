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
library(lubridate)
library(magrittr)

setwd('C:/R Projects/SimilkameenWQOs/')

# Load clean data
sim_clean <- read_csv("data/sim_gm.csv")

## mutate(DateTime=str_sub(DateTime, 1,10),
##       DateTime=as.POSIXct(DateTime, format="%Y-%m-%d", tz="UTC"))

## INITIAL DATA VISUALIZATION
## Organize dataset by units and EMS IDs to plot

### Gold Mountain Mine sites

## Set a vector of the parameters we are interested in
## Do this manually by looking at the `parameters` and `sim_clean` dataframes

##GM_Sites: "Site 13", "Site 10", "SC4", "SC3", "BC08NL0001", "BC08NL0005", "E299452"


ugL <- c("arsenic total", "arsenic dissolved", "copper total", "copper dissolved", "cadmium total",
         "cadmium dissolved", "iron total", "iron dissolved", "lead total", "lead dissolved", "nickel total", "nickel dissolved",
         "mercury total", "chromium total", "chromium dissolved", "aluminum total",
         "aluminum dissolved", "silver total", "silver dissolved", "selenium total", "selenium dissolved", "cyanide s.a.d.",
         "cyanide wad", "total cyanide", "manganese total", "manganese dissolved", "zinc total", "zinc dissolved", "molybdenum total",
         "molybdenum dissolved", "uranium total", "uranium dissolved")

mgL <- c("nitrogen ammonia total", "nitrogen dissolved nitrate", "nitrogen total nitrite",
         "nitrogen total nitrate", "nitrogen total", "nitrogen ammonia dissolved", "nitrogen total kjeldahl",
         "nitrogen total no3 & no2", "nitrogen - nitrite dissolved (no2)","nitrogen dissolved no3 & no2",
         "nitrogen total dissolved", "tss", "sulfate dissolved", "carbon total organic",
         "carbon dissolved organic", "phosphorus total", "oxygen dissolved", "hardness (dissolved)",
         "Total Dissolved Phosphorus")


## PLOT DATA 1 (in a loop, make one plot per parameter)
## First set working directory to save plots to. This section only plots with clean data. Water quality guidelines not taken into account.

setwd('C:/R Projects/SimilkameenWQOs/out/GMM')

## make dataframe of Similkameen River sites
GM_sites <- filter(sim_clean, EMS_ID == "Site 13" | EMS_ID == "Site 10" |
                     EMS_ID == "SC4" | EMS_ID ==  "SC3")


## mg/L plots
site_mgL <- filter(sim_clean, Variable %in% mgL)

for (v in mgL) {
  #change_units <- ifelse(E303845_ugL$Units == "ug/L", E303845_ugL$Value/1000, E303845_ugL$Value).
  ## This is making the values to the power of 100. Only have to change if want unit other than what guideline is expressed is.
  mgL_plots <- filter(GM_sites, Variable == v) %>%
    group_by(EMS_ID)
  plot <- ggplot(mgL_plots, aes(x = DateTime, y = Value, color = EMS_ID)) +
    #expand_limits(y = c(0, 0.5)) +
    geom_point() +
    ##geom_hline(data = mgL_plots, aes(yintercept = UpperLimit), colour = "red", linetype = "dashed", show.legend = TRUE) +
    ggtitle(v) +
    #scale_x_date(labels = date_format("%b %Y")) +
    xlab("Date") +
    ylab("mg/L")
  plot(plot)
  ggsave(filename = paste0(v,".png"), plot = plot, units= "in")

}

## ug/L plots
## convert variables from mg/L to ug/L

SR_sites <-  GM_sites %>% mutate(Value=ifelse(Units == "mg/l", Value*1000, Value )) %>%
  mutate(Units = ifelse(Units == "mg/l", "ug/l", Units))

site_ugL <- filter(sim_clean, Variable %in% ugL)


for (v in ugL) {
  mgL_plots <- filter(GM_sites, Variable == v) %>%
    group_by(EMS_ID)
  plot <- ggplot(mgL_plots, aes(x = DateTime, y = Value, color = EMS_ID)) +
    #expand_limits(y = c(0, 0.5)) +
    geom_point() +
    ##geom_hline(data = mgL_plots, aes(yintercept = UpperLimit), colour = "red", linetype = "dashed", show.legend = TRUE) +
    ggtitle(v) +
    #scale_x_date(labels = date_format("%b %Y")) +
    xlab("Date") +
    ylab("ug/L")
  plot(plot)
  ggsave(filename = paste0(v,".png"), plot = plot, units= "in")
}


## single plots of parameters in ug/L (need to change y axis) only dissolved []

## Dissolved Fe
site_Fe <- filter(GM_sites, Variable == "iron dissolved")
Feplot <- ggplot(site_Fe, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 250) +
  ggtitle("Dissolved Iron") +
  xlab("Date") +
  ylab("ug/L")
plot(Feplot)
ggsave(filename = "iron dissolved2.png", plot = Feplot, units= "in")


## Mn
site_mn <- filter(GM_sites, Variable == "manganese dissolved")
mnplot <- ggplot(site_mn, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 100) +
  ggtitle("Dissolved Mangansese") +
  xlab("Date") +
  ylab("ug/L")
plot(mnplot)
ggsave(filename = "dissolved manganese2.png", plot = mnplot, units= "in")

## Individual graphs for pH, turbidity, temp, e coli cfu/100ml (no fecal coliform)

#pH


site_pH <- filter(GM_sites, Variable == "ph")
pHplot <- ggplot(site_pH, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 13) +
  ggtitle("pH") +
  xlab("Date") +
  ylab("pH")
plot(pHplot)
ggsave(filename = "pH.png", plot = pHplot, units= "in")

## Temperature Field

site_temp <- filter(GM_sites, Variable == "temperature-field")
tempplot <- ggplot(site_temp, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 20) +
  ggtitle("Temperature - Field") +
  xlab("Date") +
  ylab("°C")
plot(tempplot)
ggsave(filename = "temp-field.png", plot = tempplot, units= "in")


## turbidity

site_tur <- filter(GM_sites, Variable == "turbidity")
turplot <- ggplot(site_tur, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point()  +
  ggtitle("Turbidity") +
  xlab("Date") +
  ylab("mg/L")
plot(turplot)
ggsave(filename = "turbidity2.png", plot = turplot, units= "in")




