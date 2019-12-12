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

setwd('C:/R Projects/Similkameen-WQOs/')

# Load clean data
sim_clean <- read_csv("data/report/sim_clean.csv")

## mutate(DateTime=str_sub(DateTime, 1,10),
##       DateTime=as.POSIXct(DateTime, format="%Y-%m-%d", tz="UTC"))

## INITIAL DATA VISUALIZATION
## Organize dataset by units and EMS IDs to plot

### Similkameen River sites

## Set a vector of the parameters we are interested in
## Do this manually by looking at the `parameters` and `sim_clean` dataframes

##SR_Sites: "0500075", "0500724", "0500725", "E207463", "BC08NL0001", "BC08NL0005", "E299452"

parameters <- distinct (sim_clean, Variable, Units, Code)

ugL <- c("arsenic total", "arsenic dissolved", "copper total", "copper dissolved", "cadmium total",
         "cadmium dissolved", "iron total", "iron dissolved", "lead total", "lead dissolved", 
         "nickel total", "nickel dissolved", "calcium total", "calcium dissolved",
         "mercury total", "chromium total", "chromium dissolved", "aluminum total",
         "aluminum dissolved", "silver total", "silver dissolved", "selenium total", 
         "selenium dissolved", "cyanide s.a.d.","cobalt total", "cobalt dissolved",
         "cyanide wad", "total cyanide", "manganese total", "manganese dissolved", "zinc total",
         "zinc dissolved", "molybdenum total", "barium total", "barium dissolved", "boron total", "boron dissolved",
         "molybdenum dissolved", "uranium total", "uranium dissolved", "strontium total", "strontium dissolved",
         )

mgL <- c("nitrogen ammonia total", "nitrogen dissolved nitrate", "nitrogen total nitrite",
         "nitrogen total nitrate", "nitrogen total", "nitrogen ammonia dissolved", "nitrogen total kjeldahl",
         "nitrogen total no3 & no2", "nitrogen - nitrite dissolved (no2)","nitrogen dissolved no3 & no2",
         "nitrogen total dissolved", "tss", "sulfate dissolved", "sulfate total", "carbon total organic",
         "carbon dissolved organic", "phosphorus total", "oxygen dissolved", "hardness (dissolved)",
         "phosphorus total dissolved", "hardness total", "hardness (calcd.) caco3", "	residue: filterable 1.0u (tds)",
         "alkalinity:total", "alkalinity total caco3", "bromide dissolved", "chloride total", "chloride dissolved",
         "carbon dissolved inorganic", "carbon total inorganic", "magnesium total", "magnesium dissolved", 
         "manganese total", "manganese dissolved", "potassium total", "potassium dissolved", "sodium total",
         "sodium dissolved")


## PLOT DATA 1 (in a loop, make one plot per parameter)
## First set working directory to save plots to. This section only plots with clean data. Water quality guidelines not taken into account.

setwd('C:/R Projects/Similkameen-WQOs/out/Report/SimilkameenRiver')

## make dataframe of Similkameen River sites
SR_sites <- filter(sim_clean, EMS_ID == "0500075" | EMS_ID == "0500724" | EMS_ID == "0500725" |
                     EMS_ID == "E207463" | EMS_ID ==  "BC08NL0001" | EMS_ID == "BC08NL0005" |
                     EMS_ID == "E299452")

## filter out the dates that I want to plot
SR_sites <- filter(SR_sites, DateTime > "2008-01-01" & DateTime < "2019-12-31")

## mg/L plots
site_mgL <- filter(sim_clean, Variable %in% mgL)

for (v in mgL) {
  #change_units <- ifelse(E303845_ugL$Units == "ug/L", E303845_ugL$Value/1000, E303845_ugL$Value).
  ## This is making the values to the power of 100. Only have to change if want unit other than what guideline is expressed is.
  mgL_plots <- filter(SR_sites, Variable == v) %>%
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

# Total Phosphorus
site_tp <- filter(SR_sites, Variable == "phosphorus total")
tpplot <- ggplot(site_tp, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 1.0) +
  ggtitle("Total Phosphorus") +
  xlab("Date") +
  ylab("mg/L")
plot(tpplot)
ggsave(filename = "total phosphorus2.png", plot = tpplot, units= "in")

## Dissolved Sulphate

site_ds <- filter(SR_sites, Variable == "sulfate dissolved")
dsplot <- ggplot(site_ds, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 30) +
  ggtitle("Dissolved Sulphate") +
  xlab("Date") +
  ylab("mg/L")
plot(dsplot)
ggsave(filename = "sulphate dissolved2.png", plot = dsplot, units= "in")

# DO
site_do <- filter(SR_sites, Variable == "oxygen dissolved")
doplot <- ggplot(site_do, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 25) +
  ggtitle("Dissolved Oxygen") +
  xlab("Date") +
  ylab("mg/L")
plot(doplot)
ggsave(filename = "oxygen dissolved2.png", plot = doplot, units= "in")

## ug/L plots
## convert variables from mg/L to ug/L

SR_sites <-  SR_sites %>% mutate(Value=ifelse(Units == "mg/l", Value*1000, Value )) %>%
  mutate(Units = ifelse(Units == "mg/l", "ug/l", Units))

site_ugL <- filter(sim_clean, Variable %in% ugL)


for (v in ugL) {
  mgL_plots <- filter(SR_sites, Variable == v) %>%
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
site_As <- filter(SR_sites, Variable == "arsenic dissolved")
Asplot <- ggplot(site_As, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 2) +
  ggtitle("Dissolved Arsenic") +
  xlab("Date") +
  ylab("ug/L")
plot(Asplot)
ggsave(filename = "arsenic dissolved2.png", plot = Asplot, units= "in")


## Dissolved Cd
site_Cd <- filter(SR_sites, Variable == "cadmium dissolved")
Cdplot <- ggplot(site_Cd, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 0.04) +
  ggtitle("Dissolved Cadmium") +
  xlab("Date") +
  ylab("ug/L")
plot(Cdplot)
ggsave(filename = "cadmium dissolved2.png", plot = Cdplot, units= "in")

## Dissolved Cu
site_Cu <- filter(SR_sites, Variable == "copper dissolved")
Cuplot <- ggplot(site_Cu, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 11) +
  ggtitle("Dissolved Copper") +
  xlab("Date") +
  ylab("ug/L")
plot(Cuplot)
ggsave(filename = "copper dissolved2.png", plot = Cuplot, units= "in")

## Dissolved Fe
site_Fe <- filter(SR_sites, Variable == "iron dissolved")
Feplot <- ggplot(site_Fe, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 750) +
  ggtitle("Dissolved Iron") +
  xlab("Date") +
  ylab("ug/L")
plot(Feplot)
ggsave(filename = "iron dissolved2.png", plot = Feplot, units= "in")

## Dissolved Pb
site_Pb <- filter(SR_sites, Variable == "lead dissolved")
Pbplot <- ggplot(site_Pb, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 1.25) +
  ggtitle("Dissolved Lead") +
  xlab("Date") +
  ylab("ug/L")
plot(Pbplot)
ggsave(filename = "lead dissolved.png", plot = Pbplot, units= "in")

## Dissolved Ni
site_Ni <- filter(SR_sites, Variable == "nickel dissolved")
Niplot <- ggplot(site_Ni, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 3) +
  ggtitle("Dissolved Nickel") +
  xlab("Date") +
  ylab("ug/L")
plot(Niplot)
ggsave(filename = "nickel dissolved2.png", plot = Niplot, units= "in")

## Dissolved Se
site_Se <- filter(SR_sites, Variable == "selenium dissolved")
Seplot <- ggplot(site_Se, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 0.6) +
  ggtitle("Dissolved Selenium") +
  xlab("Date") +
  ylab("ug/L")
plot(Seplot)
ggsave(filename = "selenium dissolved2.png", plot = Seplot, units= "in")

## Dissolved Ag
site_Ag <- filter(SR_sites, Variable == "silver dissolved")
Agplot <- ggplot(site_Ag, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 0.025) +
  ggtitle("Dissolved Silver") +
  xlab("Date") +
  ylab("ug/L")
plot(Agplot)
ggsave(filename = "silver dissolved2.png", plot = Agplot, units= "in")

## Dissolved Zinc
site_Zn <- filter(SR_sites, Variable == "zinc dissolved")
Znplot <- ggplot(site_Zn, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 13) +
  ggtitle("Dissolved Zinc") +
  xlab("Date") +
  ylab("ug/L")
plot(Znplot)
ggsave(filename = "zinc dissolved2.png", plot = Znplot, units= "in")

## Dissolved Al
site_Al <- filter(SR_sites, Variable == "aluminum dissolved")
Alplot <- ggplot(site_Al, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 500) +
  ggtitle("Dissolved Aluminum") +
  xlab("Date") +
  ylab("ug/L")
plot(Alplot)
ggsave(filename = "aluminum dissolved2.png", plot = Alplot, units= "in")

## Dissolved Mb
site_Mb <- filter(SR_sites, Variable == "molybdenum dissolved")
Mbplot <- ggplot(site_Mb, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 4.5) +
  ggtitle("Dissolved Molybdenum") +
  xlab("Date") +
  ylab("ug/L")
plot(Mbplot)
ggsave(filename = "molybdenum dissolved2.png", plot = Mbplot, units= "in")

## CN- WAD
site_wad <- filter(SR_sites, Variable == "cyanide wad")
wadplot <- ggplot(site_wad, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 1.5) +
  ggtitle("Cyanide WAD") +
  xlab("Date") +
  ylab("ug/L")
plot(wadplot)
ggsave(filename = "cyanidewad2.png", plot = wadplot, units= "in")

## Mn
site_mn <- filter(SR_sites, Variable == "manganese dissolved")
mnplot <- ggplot(site_mn, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 15) +
  ggtitle("Dissolved Mangansese") +
  xlab("Date") +
  ylab("ug/L")
plot(mnplot)
ggsave(filename = "dissolved manganese2.png", plot = mnplot, units= "in")

## Cr
site_cr <- filter(SR_sites, Variable == "chromium dissolved")
crplot <- ggplot(site_cr, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 0.75) +
  ggtitle("Dissolved Chromium") +
  xlab("Date") +
  ylab("ug/L")
plot(crplot)
ggsave(filename = "chromium dissolved2.png", plot = mnplot, units= "in")

## Individual graphs for pH, turbidity, temp, e coli cfu/100ml (no fecal coliform)

#pH


site_pH <- filter(SR_sites, Variable == "ph")
pHplot <- ggplot(site_pH, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 13) +
  ggtitle("pH") +
  xlab("Date") +
  ylab("pH")
plot(pHplot)
ggsave(filename = "pH.png", plot = pHplot, units= "in")

## Temperature

site_temp <- filter(SR_sites, Variable == "temperature water")
tempplot <- ggplot(site_temp, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 25) +
  ggtitle("Temperature") +
  xlab("Date") +
  ylab("°C")
plot(tempplot)
ggsave(filename = "temp.png", plot = tempplot, units= "in")

## Temperature Field

site_temp <- filter(SR_sites, Variable == "temperature-field")
tempplot <- ggplot(site_temp, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 20) +
  ggtitle("Temperature - Field") +
  xlab("Date") +
  ylab("°C")
plot(tempplot)
ggsave(filename = "temp-field.png", plot = tempplot, units= "in")


## E Coli

site_ecoli <- filter(SR_sites, Variable == "e coli", Units == "cfu/100ml")
ecoliplot2 <- ggplot(site_ecoli, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() +
  ggtitle("E. Coli.") +
  xlab("Date") +
  ylab("cfu/100mL")
plot(ecoliplot2)
ggsave(filename = "ecoli.png", plot = ecoliplot2, units= "in")

# E Coli2

site_ecoli <- filter(SR_sites, Variable == "e coli", Units == "cfu/100ml")
ecoliplot2 <- ggplot(site_ecoli, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0, 45) +
  ggtitle("E. Coli.") +
  xlab("Date") +
  ylab("cfu/100mL")
plot(ecoliplot2)
ggsave(filename = "ecoli2.png", plot = ecoliplot2, units= "in")


## turbidity

site_tur <- filter(SR_sites, Variable == "turbidity")
turplot <- ggplot(site_tur, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point()  + ylim(0, 100)+
  ggtitle("Turbidity") +
  xlab("Date") +
  ylab("mg/L")
plot(turplot)
ggsave(filename = "turbidity2.png", plot = turplot, units= "in")

## Fecal Coliform
site_fecal <- filter(SR_sites, Variable == "coliform - fecal", Units == "cfu/100ml")
fecalplot2 <- ggplot(site_fecal, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() +
  ggtitle("Fecal Coliform") +
  xlab("Date") +
  ylab("cfu/100mL")
plot(fecalplot2)
ggsave(filename = "fecal coliform2.png", plot = fecalplot2, units= "in")

## Enterococcus
site_ent <- filter(SR_sites, Variable == "enterococcus", Units == "cfu/100ml")
entplot2 <- ggplot(site_ent, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() +
  ggtitle("Enterococcus") +
  xlab("Date") +
  ylab("cfu/100mL")
plot(entplot2)
ggsave(filename = "enterococcus.png", plot = entplot2, units= "in")


