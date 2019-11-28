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

## INITIAL DATA VISUALIZATION
## Organize dataset by units and EMS IDs to plot

### Keremeos Creek sites

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

setwd('C:/R Projects/SimilkameenWQOs/out/KeremeosCreek/Recent/')

## make dataframe of Keremeos Creek sites
KC_sites <- filter(sim_clean, EMS_ID == "E221390" | EMS_ID == "E221387" | EMS_ID == "E221386" |
                     EMS_ID == "E221413" | EMS_ID ==  "E221384" | EMS_ID == "E221389" |
                     EMS_ID == "0500757" | EMS_ID == "E221340" | EMS_ID == "E221339" |
                     EMS_ID == "E221341")

## filter out the dates that I want to plot
KC_sites <- filter(KC_sites, DateTime > "2008-01-01" & DateTime < "2019-12-31")


## mg/L plots
site_mgL <- filter(KC_sites, Variable %in% mgL)

 for (v in mgL) {
  #change_units <- ifelse(E303845_ugL$Units == "ug/L", E303845_ugL$Value/1000, E303845_ugL$Value).
  ## This is making the values to the power of 100. Only have to change if want unit other than what guideline is expressed is.
  mgL_plots <- filter(KC_sites, Variable == v) %>%
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

## ug/L plots
## convert variables from mg/L to ug/L

KC_sites <-  KC_sites %>% mutate(Value=ifelse(Units == "mg/l", Value*1000, Value )) %>%
  mutate(Units = ifelse(Units == "mg/l", "ug/l", Units))

site_ugL <- filter(KC_sites, Variable %in% ugL)


for (v in ugL) {
  mgL_plots <- filter(KC_sites, Variable == v) %>%
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


## Individual graphs for pH, turbidity, temp, e coli cfu/100ml (no fecal coliform)

#pH

site_pH <- filter(KC_sites, Variable == "ph")
pHplot <- ggplot(site_pH, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() +
  ggtitle("pH") +
  xlab("Date") +
  ylab("pH")
plot(pHplot)
ggsave(filename = "pHplot.png", plot = pHplot, units= "in")

## Temperature

site_temp <- filter(KC_sites, Variable == "temperature")
tempplot <- ggplot(site_temp, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() +
  ggtitle("Temperature") +
  xlab("Date") +
  ylab("°C")
plot(tempplot)
ggsave(filename = "tempplot.png", plot = tempplot, units= "in")

## E Coli

site_ecoli <- filter(KC_sites, Variable == "e coli", Units == "cfu/100ml")
ecoliplot <- ggplot(site_ecoli, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + ylim(0,500)
  ggtitle("E. Coli.") +
  xlab("Date") +
  ylab("cfu/100mL")
plot(ecoliplot)
ggsave(filename = "ecoliplot2.png", plot = ecoliplot, units= "in")

## turbidity

site_tur <- filter(KC_sites, Variable == "turbidity")
turplot <- ggplot(site_tur, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point()  +
  ggtitle("Turbidity") +
  xlab("Date") +
  ylab("NTU")
plot(turplot)
ggsave(filename = "turbidity2.png", plot = turplot, units= "in")

## Fecal Coliform
site_fecal <- filter(KC_sites, Variable == "coliform - fecal", Units == "cfu/100ml")
fecalplot2 <- ggplot(site_fecal, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() +
  ggtitle("Fecal Coliform") +
  xlab("Date") +
  ylab("cfu/100mL")
plot(fecalplot2)
ggsave(filename = "fecal coliform2.png", plot = fecalplot2, units= "in")

## Enterococcus
site_ent <- filter(KC_sites, Variable == "enterococcus", Units == "cfu/100ml")
entplot2 <- ggplot(site_ent, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() +
  ggtitle("Enterococcus") +
  xlab("Date") +
  ylab("cfu/100mL")
plot(entplot2)
ggsave(filename = "enterococcus.png", plot = entplot2, units= "in")
