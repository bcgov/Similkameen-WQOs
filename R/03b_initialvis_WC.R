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
##      DateTime=as.POSIXct(DateTime, format="%Y-%m-%d", tz="UTC"))


## INITIAL DATA VISUALIZATION
## Organize dataset by units and EMS IDs to plot

### Wolfe Creek sites

## Set a vector of the parameters we are interested in
## Do this manually by looking at the `parameters` and `sim_clean` dataframes


## Look at parameters and units
#parameters <- distinct (sim_clean, Variable)
##untis <- distinct (sim_clean, Units)

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

setwd('C:/R Projects/SimilkameenWQOs/out/WolfeCreek/Recent/')

## make dataframe of Wolfe Creek sites
WC_sites <- filter(sim_clean, EMS_ID == "E266462" | EMS_ID == "0500101")

## filter out the dates that I want to plot
WC_sites <- filter(WC_sites, DateTime > "2008-01-01" & DateTime < "2019-12-31")

## mg/L plots (mg/L plots first since I will have to change all mg/L to ug/L for that set of plots)
site_mgL <- filter(WC_sites, Variable %in% mgL)

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

## Ammonia

site_NH3 <- filter(WC_sites, Variable == "nitrogen ammonia total")
NH3plot <- ggplot(site_NH3, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 0.045) +
  ggtitle("Total Ammonia") +
  xlab("Date") +
  ylab("mg/L")
plot(NH3plot)
ggsave(filename = "nitrogen ammonia total2.png", plot = NH3plot, units= "in")

# TSS
site_TSS <- filter(WC_sites, Variable == "tss")
TSSplot <- ggplot(site_TSS, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 75) +
  ggtitle("TSS") +
  xlab("Date") +
  ylab("mg/L")
plot(TSSplot)
ggsave(filename = "TSS2.png", plot = TSSplot, units= "in")


## ug/L plots
## convert variables from mg/L to ug/L

WC_sites <-  WC_sites %>% mutate(Value=ifelse(Units == "mg/l", Value*1000, Value )) %>%
  mutate(Units = ifelse(Units == "mg/l", "ug/l", Units))

site_ugL <- filter(WC_sites, Variable %in% ugL)


for (v in ugL) {
  ugL_plots <- filter(WC_sites, Variable == v) %>%
    group_by(EMS_ID)
  plot <- ggplot(ugL_plots, aes(x = DateTime, y = Value, color = EMS_ID)) +
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
site_As <- filter(WC_sites, Variable == "arsenic dissolved")
Asplot <- ggplot(site_As, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 1.2) +
  ggtitle("Dissolved Arsenic") +
  xlab("Date") +
  ylab("ug/L")
plot(Asplot)
ggsave(filename = "arsenic dissolved2.png", plot = Asplot, units= "in")


## Dissolved Cd
site_Cd <- filter(WC_sites, Variable == "cadmium dissolved")
Cdplot <- ggplot(site_Cd, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 0.04) +
  ggtitle("Dissolved Cadmium") +
  xlab("Date") +
  ylab("ug/L")
plot(Cdplot)
ggsave(filename = "cadmium dissolved2.png", plot = Cdplot, units= "in")

## Dissolved Cu
site_Cu <- filter(WC_sites, Variable == "copper dissolved")
Cuplot <- ggplot(site_Cu, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 11) +
  ggtitle("Dissolved Copper") +
  xlab("Date") +
  ylab("ug/L")
plot(Cuplot)
ggsave(filename = "copper dissolved2.png", plot = Cuplot, units= "in")


## Dissolved Pb
site_Pb <- filter(WC_sites, Variable == "lead dissolved")
Pbplot <- ggplot(site_Pb, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 0.075) +
  ggtitle("Dissolved Lead") +
  xlab("Date") +
  ylab("ug/L")
plot(Pbplot)
ggsave(filename = "lead dissolved2.png", plot = Pbplot, units= "in")

## Dissolved Ni
site_Ni <- filter(WC_sites, Variable == "nickel dissolved")
Niplot <- ggplot(site_Ni, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 1.2) +
  ggtitle("Dissolved Nickel") +
  xlab("Date") +
  ylab("ug/L")
plot(Niplot)
ggsave(filename = "nickel dissolved2.png", plot = Niplot, units= "in")

## Dissolved Se
site_Se <- filter(WC_sites, Variable == "selenium dissolved")
Seplot <- ggplot(site_Se, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 2.5) +
  ggtitle("Dissolved Selenium") +
  xlab("Date") +
  ylab("ug/L")
plot(Seplot)
ggsave(filename = "selenium dissolved2.png", plot = Seplot, units= "in")

## Dissolved Ag
site_Ag <- filter(WC_sites, Variable == "silver dissolved")
Agplot <- ggplot(site_Ag, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 0.03) +
  ggtitle("Dissolved Silver") +
  xlab("Date") +
  ylab("ug/L")
plot(Agplot)
ggsave(filename = "silver dissolved2.png", plot = Agplot, units= "in")

## Dissolved Zinc
site_Zn <- filter(WC_sites, Variable == "zinc dissolved")
Znplot <- ggplot(site_Zn, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 6) +
  ggtitle("Dissolved Zinc") +
  xlab("Date") +
  ylab("ug/L")
plot(Znplot)
ggsave(filename = "zinc dissolved2.png", plot = Znplot, units= "in")

## Dissolved Hg
site_Hg <- filter(WC_sites, Variable == "mercury total")
Hgplot <- ggplot(site_Hg, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 0.03) +
  ggtitle("Total Mercury") +
  xlab("Date") +
  ylab("ug/L")
plot(Hgplot)
ggsave(filename = "mercury total2.png", plot = Alplot, units= "in")


## Individual graphs for pH, turbidity, temp, e coli cfu/100ml (no fecal coliform
## or enterococcus)

#pH

site_pH <- filter(WC_sites, Variable == "ph")
pHplot <- ggplot(site_pH, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 13) +
  ggtitle("pH") +
  xlab("Date") +
  ylab("pH")
plot(pHplot)
ggsave(filename = "pHplot.png", plot = pHplot, units= "in")

## Temperature

site_temp <- filter(WC_sites, Variable == "temperature")
tempplot <- ggplot(site_temp, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 25) +
  ggtitle("Temperature") +
  xlab("Date") +
  ylab("°C")
plot(tempplot)
ggsave(filename = "tempplot.png", plot = tempplot, units= "in")

## Temperature Field

site_temp <- filter(WC_sites, Variable == "temperature-field")
tempplot <- ggplot(site_temp, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 20) +
  ggtitle("Temperature - Field") +
  xlab("Date") +
  ylab("°C")
plot(tempplot)
ggsave(filename = "2013 temp-field.png", plot = tempplot, units= "in")


## E Coli

site_ecoli <- filter(WC_sites, Variable == "e coli", Units == "cfu/100ml")
ecoliplot2 <- ggplot(site_ecoli, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() +
  ggtitle("E. Coli.") +
  xlab("Date") +
  ylab("cfu/100mL")
plot(ecoliplot2)
ggsave(filename = "2013 ecoli.png", plot = ecoliplot2, units= "in")


## turbidity

site_tur <- filter(WC_sites, Variable == "turbidity")
turplot <- ggplot(site_tur, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point()  +
  ggtitle("Turbidity") +
  xlab("Date") +
  ylab("NTU")
plot(turplot)
ggsave(filename = "turbidity2.png", plot = turplot, units= "in")

