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

### CMM Upper Wolfe Creek sites

## Set a vector of the parameters we are interested in
## Do this manually by looking at the `parameters` and `sim_clean` dataframes


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

setwd('C:/R Projects/SimilkameenWQOs/out/CMM/UWC')

## make dataframe of Upper Wolfe Creek Sites
WC_sites <- filter(sim_clean, EMS_ID == "E307368" | EMS_ID == "E287171" | EMS_ID == "E266462" |
                     EMS_ID == "E287173" | EMS_ID ==  "E287174" | EMS_ID == "E287175" |
                     EMS_ID == "E287176" | EMS_ID ==  "E306784" | EMS_ID ==  "E254310" |
                     EMS_ID ==  "0500397" )

## filter out the dates that I want to plot
WC_sites <- filter(WC_sites, DateTime > "2008-01-01" & DateTime < "2019-12-31")

## mg/L plots
site_mgL <- filter(sim_clean, Variable %in% mgL)

for (v in mgL) {
  #change_units <- ifelse(E303845_ugL$Units == "ug/L", E303845_ugL$Value/1000, E303845_ugL$Value).
  ## This is making the values to the power of 100. Only have to change if want unit other than what guideline is expressed is.
  mgL_plots <- filter(WC_sites, Variable == v) %>%
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

## single plots of parameters in mg/L (need to change y axis)

# DOC
site_doc <- filter(WC_sites, Variable == "carbon dissolved organic")
docplot <- ggplot(site_doc, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 50) +
  ggtitle("Dissolved Organic Carbon") +
  xlab("Date") +
  ylab("mg/L")
plot(docplot)
ggsave(filename = "carbon dissolved organic2.png", plot = docplot, units= "in")

## Dissolved Sulphate

site_tss <- filter(WC_sites, Variable == "tss")
tssplot <- ggplot(site_tss, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 300) +
  ggtitle("TSS") +
  xlab("Date") +
  ylab("mg/L")
plot(tssplot)
ggsave(filename = "tss2.png", plot = tssplot, units= "in")

## ug/L plots
## convert variables from mg/L to ug/L

WC_sites <-  WC_sites %>% mutate(Value=ifelse(Units == "mg/l", Value*1000, Value )) %>%
  mutate(Units = ifelse(Units == "mg/l", "ug/l", Units))

site_ugL <- filter(sim_clean, Variable %in% ugL)


for (v in ugL) {
  mgL_plots <- filter(WC_sites, Variable == v) %>%
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

## Dissolved Cu
site_Cu <- filter(WC_sites, Variable == "copper dissolved")
Cuplot <- ggplot(site_Cu, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 30) +
  ggtitle("Dissolved Copper") +
  xlab("Date") +
  ylab("ug/L")
plot(Cuplot)
ggsave(filename = "copper dissolved2.png", plot = Cuplot, units= "in")

## Dissolved Fe
site_Fe <- filter(WC_sites, Variable == "iron dissolved")
Feplot <- ggplot(site_Fe, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 600) +
  ggtitle("Dissolved Iron") +
  xlab("Date") +
  ylab("ug/L")
plot(Feplot)
ggsave(filename = "iron dissolved2.png", plot = Feplot, units= "in")


## Dissolved Al
site_Al <- filter(WC_sites, Variable == "aluminum dissolved")
Alplot <- ggplot(site_Al, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 350) +
  ggtitle("Dissolved Aluminum") +
  xlab("Date") +
  ylab("ug/L")
plot(Alplot)
ggsave(filename = "aluminum dissolved2.png", plot = Alplot, units= "in")


## Individual graphs for pH, turbidity, temp, e coli cfu/100ml (no fecal coliform)

#pH


site_pH <- filter(WC_sites, Variable == "ph")
pHplot <- ggplot(site_pH, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 13) +
  ggtitle("pH") +
  xlab("Date") +
  ylab("pH")
plot(pHplot)
ggsave(filename = "pH.png", plot = pHplot, units= "in")

## Temperature Field

site_temp <- filter(WC_sites, Variable == "temperature-field")
tempplot <- ggplot(site_temp, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 20) +
  ggtitle("Temperature - Field") +
  xlab("Date") +
  ylab("°C")
plot(tempplot)
ggsave(filename = "temp-field.png", plot = tempplot, units= "in")


## E Coli

site_ecoli <- filter(WC_sites, Variable == "e coli", Units == "cfu/100ml")
ecoliplot2 <- ggplot(site_ecoli, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() +
  ggtitle("E. Coli.") +
  xlab("Date") +
  ylab("cfu/100mL")
plot(ecoliplot2)
ggsave(filename = "ecoli.png", plot = ecoliplot2, units= "in")

# E Coli2

site_ecoli <- filter(WC_sites, Variable == "e coli", Units == "cfu/100ml")
ecoliplot2 <- ggplot(site_ecoli, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 45) +
  ggtitle("E. Coli.") +
  xlab("Date") +
  ylab("cfu/100mL")
plot(ecoliplot2)
ggsave(filename = "ecoli2.png", plot = ecoliplot2, units= "in")


## turbidity

site_tur <- filter(WC_sites, Variable == "turbidity")
turplot <- ggplot(site_tur, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point()  + ylim(0, 100)+
  ggtitle("Turbidity") +
  xlab("Date") +
  ylab("mg/L")
plot(turplot)
ggsave(filename = "turbidity2.png", plot = turplot, units= "in")




