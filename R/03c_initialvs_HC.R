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

## Load packages from script 01_load.R if needed

setwd('C:/R Projects/SimilkameenWQOs/')

# Load clean data
sim_clean <- read_csv("data/sim_gm.csv")

## mutate(DateTime=str_sub(DateTime, 1,10),
##       DateTime=as.POSIXct(DateTime, format="%Y-%m-%d", tz="UTC"))

## INITIAL DATA VISUALIZATION
## Organize dataset by units and EMS IDs to plot

### Hedley Creek sites

## Set a vector of the parameters we are interested in
## Do this manually by looking at the `parameters` and `sim_clean` dataframes


ugL <- c("arsenic total", "arsenic dissolved", "cobalt total", "cobalt dissolved", "copper total", "copper dissolved", "cadmium total",
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

setwd('C:/R Projects/SimilkameenWQOs/out/HedleyCreek/Recent/')

## make dataframe of Hedley Creek sites
HC_sites <- filter(sim_clean, EMS_ID == "E223873" | EMS_ID == "E223874" )

## filter out the dates that I want to plot
HC_sites <- filter(HC_sites, DateTime > "2008-01-01" & DateTime < "2019-12-31")

## mg/L plots
site_mgL <- filter(HC_sites, Variable %in% mgL)

for (v in mgL) {
  #change_units <- ifelse(E303845_ugL$Units == "ug/L", E303845_ugL$Value/1000, E303845_ugL$Value).
  ## This is making the values to the power of 100. Only have to change if want unit other than what guideline is expressed is.
  mgL_plots <- filter(HC_sites, Variable == v) %>%
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

## Dissolved Sulphate

site_ds <- filter(HC_sites, Variable == "sulfate dissolved")
dsplot <- ggplot(site_ds, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 80) +
  ggtitle("Dissolved Sulphate") +
  xlab("Date") +
  ylab("mg/L")
plot(dsplot)
ggsave(filename = "sulphate dissolved2.png", plot = dsplot, units= "in")


## ug/L plots
## convert variables from mg/L to ug/L

HC_sites <-  HC_sites %>% mutate(Value=ifelse(Units == "mg/l", Value*1000, Value )) %>%
  mutate(Units = ifelse(Units == "mg/l", "ug/l", Units))

site_ugL <- filter(HC_sites, Variable %in% ugL)


for (v in ugL) {
  mgL_plots <- filter(HC_sites, Variable == v) %>%
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

## Dissolved As
site_As <- filter(HC_sites, Variable == "arsenic dissolved")
Asplot <- ggplot(site_As, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 3) +
  ggtitle("Dissolved Arsenic") +
  xlab("Date") +
  ylab("ug/L")
plot(Asplot)
ggsave(filename = "arsenic dissolved2.png", plot = Asplot, units= "in")


## Dissolved Cd
site_Cd <- filter(HC_sites, Variable == "cadmium dissolved")
Cdplot <- ggplot(site_Cd, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 0.25) +
  ggtitle("Dissolved Cadmium") +
  xlab("Date") +
  ylab("ug/L")
plot(Cdplot)
ggsave(filename = "cadmium dissolved2.png", plot = Cdplot, units= "in")

## Dissolved Cr
site_Cr <- filter(HC_sites, Variable == "chromium dissolved")
Crplot <- ggplot(site_Cr, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0,1.1) +
  ggtitle("Dissolved Chromium") +
  xlab("Date") +
  ylab("ug/L")
plot(Crplot)
ggsave(filename = "chromium dissolved2.png", plot = Crplot, units= "in")


## Dissolved Pb
site_Pb <- filter(HC_sites, Variable == "lead dissolved")
Pbplot <- ggplot(site_Pb, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 10) +
  ggtitle("Dissolved Lead") +
  xlab("Date") +
  ylab("ug/L")
plot(Pbplot)
ggsave(filename = "lead dissolved2.png", plot = Pbplot, units= "in")

## Dissolved Mn
site_Mn <- filter(HC_sites, Variable == "manganese dissolved")
Mnplot <- ggplot(site_Mn, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 3.5) +
  ggtitle("Dissolved Managanese") +
  xlab("Date") +
  ylab("ug/L")
plot(Mnplot)
ggsave(filename = "manganese dissolved2.png", plot = Mnplot, units= "in")

## Dissolved Mo
site_Mo <- filter(HC_sites, Variable == "molybdenum dissolved")
Moplot <- ggplot(site_Mo, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 3) +
  ggtitle("Dissolved Molybdenum") +
  xlab("Date") +
  ylab("ug/L")
plot(Moplot)
ggsave(filename = "molybdenum dissolved2.png", plot = Moplot, units= "in")

## Dissolved Ni
site_Ni <- filter(HC_sites, Variable == "nickel dissolved")
Niplot <- ggplot(site_Ni, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 1) +
  ggtitle("Dissolved Nickel") +
  xlab("Date") +
  ylab("ug/L")
plot(Niplot)
ggsave(filename = "nickel dissolved2.png", plot = Niplot, units= "in")

## Dissolved Se
site_Se <- filter(HC_sites, Variable == "selenium dissolved")
Seplot <- ggplot(site_Se, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 3) +
  ggtitle("Dissolved Selenium") +
  xlab("Date") +
  ylab("ug/L")
plot(Seplot)
ggsave(filename = "selenium dissolved2.png", plot = Seplot, units= "in")

## Dissolved Ag
site_Ag <- filter(HC_sites, Variable == "silver dissolved")
Agplot <- ggplot(site_Ag, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 0.1) +
  ggtitle("Dissolved Silver") +
  xlab("Date") +
  ylab("ug/L")
plot(Agplot)
ggsave(filename = "silver dissolved2.png", plot = Agplot, units= "in")

## Dissolved Co
site_Co <- filter(HC_sites, Variable == "cobalt dissolved")
Coplot <- ggplot(site_Co, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() +
  ggtitle("Dissolved Cobalt") +
  xlab("Date") +
  ylab("ug/L")
plot(Coplot)
ggsave(filename = "Cobalt dissolved.png", plot = Coplot, units= "in")

## Total Co
site_Co <- filter(HC_sites, Variable == "cobalt total")
Coplot <- ggplot(site_Co, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() +
  ggtitle("Total Cobalt") +
  xlab("Date") +
  ylab("ug/L")
plot(Coplot)
ggsave(filename = "Cobalt Total.png", plot = Coplot, units= "in")

## Individual graphs for pH, turbidity, temp, e coli cfu/100ml

#pH


site_pH <- filter(HC_sites, Variable == "ph")
pHplot <- ggplot(site_pH, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 13) +
  ggtitle("pH") +
  xlab("Date") +
  ylab("pH")
plot(pHplot)
ggsave(filename = "pHplot.png", plot = pHplot, units= "in")

## Temperature

site_temp <- filter(HC_sites, Variable == "temperature")
tempplot <- ggplot(site_temp, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() +
  ggtitle("Temperature") +
  xlab("Date") +
  ylab("°C")
plot(tempplot)
ggsave(filename = "tempplot.png", plot = tempplot, units= "in")


## E Coli

site_ecoli <- filter(HC_sites, Variable == "e coli", Units == "cfu/100ml")
ecoliplot2 <- ggplot(site_ecoli, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() +
  ggtitle("E. Coli.") +
  xlab("Date") +
  ylab("cfu/100mL")
plot(ecoliplot2)
ggsave(filename = "2013ecoliplot.png", plot = ecoliplot2, units= "in")


## turbidity

site_tur <- filter(HC_sites, Variable == "turbidity")
turplot <- ggplot(site_tur, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point()  +
  ggtitle("Turbidity") +
  xlab("Date") +
  ylab("NTU")
plot(turplot)
ggsave(filename = "turbidity2.png", plot = turplot, units= "in")

