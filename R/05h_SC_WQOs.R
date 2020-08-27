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


# This script will graph the parameters for the Sunset Creek, plot WQGs/WQOs

## Load packages 

library(tidyverse)
library(lubridate)
library(magrittr)
library(plotly)


setwd('C:/R Projects/Similkameen-WQOs/')

# Load data that is ready for plotting with WQOs
plotdata <- read_csv("data/report/SC_Plots/tables/SC_sites2.csv")

NH4WQGs <- read_csv(file="data/report/NH4_guidelines_long.csv",
                    col_types = cols_only(EMS_ID=col_character(),
                                          DateTime=col_character(),
                                          ResultLetter=col_character(),
                                          Variable=col_factor(),
                                          Value=col_double(),
                                          CENSOR=col_factor() )) %>%
  mutate(
    DateTime=as.POSIXct(DateTime, format="%Y-%m-%d", tz="UTC"))


CuWQGs <- read_csv(file="data/report/CuWQGs_long.csv",
                   col_types = cols_only(DateTime=col_character(),
                                         EMS_ID=col_character(),
                                         ResultLetter=col_character(),
                                         CENSOR=col_factor(),
                                         Variable=col_factor(),
                                         Value=col_double() )) %>%
  mutate(
    DateTime=as.POSIXct(DateTime, format="%Y-%m-%d", tz="UTC"))


## change case of censor column
plotdata$CENSOR <- gsub("FALSE", "False", plotdata$CENSOR)
plotdata$CENSOR <- gsub("TRUE", "True", plotdata$CENSOR)


## order sites from upstream to downstream
plotdata$EMS_ID <- factor(plotdata$EMS_ID, 
                          levels=c( "E215954", "E250751",
                                      "E206634"))

#units <- distinct(plotdata, Units)

# set working drive for plots
setwd('C:/R Projects/Similkameen-WQOs/data/report/SC_Plots/WQOs')

## PLOTTING

#TDS
site_tds <- filter(plotdata, Variable %in% "tds")
#filter out data from 2015 to present
site_tds <- filter(site_tds, DateTime > "2015-01-01" & DateTime < "2019-12-31")
tdsplot <- ggplot(site_tds, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  xlab("Date") +
  ylab("TDS (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")
plot(tdsplot)
ggsave(filename = "tds2.png", plot = tdsplot, h=4, w=6, units="in", dpi=300)

#TSS
site_tss <- filter(plotdata, Variable %in% "tss")
tssplot <- ggplot(site_tss, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  xlab("Date") +
  ylab("TSS (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")
plot(tssplot)
ggsave(filename = "tss.png", plot = tssplot, h=4, w=6, units="in", dpi=300)

#facet for TSS

site_tss <- filter(plotdata, Variable %in% "tss")
tssplot <- ggplot(site_tss, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("TSS (mg/L)") + coord_cartesian(ylim = c(0,25)) +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(tssplot)
ggsave(filename = "tss_facet.png", plot = tssplot, h=4, w=6, units="in", dpi=300)

# load TSS function and then run

(tsslimits<-TSS_WQO(plotdata, "E215954", 20, 20, 100))

#write csv of TSS threshold, data shows clear flows for threshold is 10 mg/L 
write.csv(tsslimits,'C:/R Projects/Similkameen-WQOs/data/report/SC_Plots/tables/SR_tss.csv', row.names = FALSE)

## plot tss against threshold for E221413

tssplot <- ggplot(site_tss, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) +
  geom_line(data=tsslimits %>% select(-EMS_ID), aes(x = DateTime, y=Threshold), color="darkred") +
  theme_bw() +
  xlab("Date") +
  ylab("TSS (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(tssplot)
ggsave(filename = "tss_facetWQO.png", plot = tssplot, h=4, w=6, units="in", dpi=300)

#Dissolved Calcium
site_ca <- filter(plotdata, Variable %in% "calcium dissolved")
caplot <- ggplot(site_ca, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key= element_rect(fill="NA")) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("Dissolved Calcium (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(caplot)
ggsave(filename = "D Ca.png", plot = caplot, h=4, w=6, units="in", dpi=300)

# Facet for Dissolved Calcium

site_ca <- filter(plotdata, Variable %in% "calcium dissolved")
caplot <- ggplot(site_ca, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Calcium (mg/L)") +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(caplot)
ggsave(filename = "D Ca_facet.png", plot = caplot, h=4, w=6, units="in", dpi=300)

#Dissolved Magnesium
site_mg <- filter(plotdata, Variable %in% "magnesium dissolved")
mgplot <- ggplot(site_mg, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key= element_rect(fill="NA")) + 
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("Dissolved Magnesium (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(mgplot)
ggsave(filename = "D MG.png", plot = mgplot, h=4, w=6, units="in", dpi=300)

# Facet for Dissolved Magnesium

site_mg <- filter(plotdata, Variable %in% "magnesium dissolved")
mgplot <- ggplot(site_mg, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Magnesium (mg/L)") +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(mgplot)
ggsave(filename = "D Mg_facet.png", plot = mgplot, h=4, w=6, units="in", dpi=300)


#Dissolved Hardness (calculated from Mg and Ca)
site_hard <- filter(plotdata, Variable %in% "hardcalc")
hardplot <- ggplot(site_hard, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point(size=0.75) + 
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key= element_rect(fill="NA")) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("Dissolved Hardness (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(hardplot)
ggsave(filename = "hardnessD.png", plot = hardplot, h=4, w=6, units="in", dpi=300)

# Facet for Dissolved Hardness

site_hard <- filter(plotdata, Variable %in% "hardcalc")
hardplot <- ggplot(site_hard, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Hardness (mg/L)") +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(hardplot)
ggsave(filename = "hardnessD_facet.png", plot = hardplot, h=4, w=6, units="in", dpi=300)

#Total Dissolved Phosphorus

site_P <- filter(plotdata, Variable %in% "phosphorus total dissolved")
Pplot <- ggplot(site_P, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key= element_rect(fill="NA")) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("Total Dissolved Phosphorus (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")
plot(Pplot)
ggsave(filename = "DP.png", plot = Pplot, h=4, w=6, units="in", dpi=300)


# dissolved Cr facet

site_Cr <- filter(plotdata, Variable %in% "chromium dissolved")
Crplot <- ggplot(site_Cr, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Chromium"~~(mu*g/L)) + coord_cartesian(ylim = c(0,0.25)) +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(Crplot)
ggsave(filename = "DCr_facet.png", plot = Crplot, h=4, w=6, units="in", dpi=300)

# Cyanide WAD

site_wad <- filter(plotdata, Variable %in% c("cyanide wad" ))
wadplot <- ggplot(site_wad, aes(x = DateTime, y = Value, color = EMS_ID, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("Cyanide W.A.D."~~(mu*g/L)) + 
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")
plot(wadplot)
ggsave(filename = "CnWAD.png", plot = wadplot, h=4, w=6, units="in", dpi=300)

# Cyanide WAD facet

site_wad <- filter(plotdata, Variable %in% "cyanide wad")
wadplot <- ggplot(site_wad, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Cyanide W.A.D."~~(mu*g/L)) + coord_cartesian(ylim = c(0,10))+
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(wadplot)
ggsave(filename = "CnWAD_facet.png", plot = wadplot, h=4, w=6, units="in", dpi=300)

# Cyanide SAD

site_sad <- filter(plotdata, Variable %in% c("cyanide s.a.d." ))
sadplot <- ggplot(site_sad, aes(x = DateTime, y = Value, color = EMS_ID, shape = CENSOR)) +
  geom_point(size=0.75) +
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("Cyanide S.A.D."~~(mu*g/L)) + 
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")
plot(sadplot)
ggsave(filename = "CnSAD.png", plot = sadplot, h=4, w=6, units="in", dpi=300)

# Cyanide SAD facet

site_sad <- filter(plotdata, Variable %in% "cyanide s.a.d.")
sadplot <- ggplot(site_sad, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Cyanide S.A.D."~~(mu*g/L)) + coord_cartesian(ylim = c(0,20))+
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(sadplot)
ggsave(filename = "CnSAD_facet.png", plot = sadplot, h=4, w=6, units="in", dpi=300)


# Total and Dissolved Arsenic

plotnames <- c('arsenic total' = "T-As", 'arsenic dissolved' = "D-As")

site_as <- filter(plotdata, Variable %in% c("arsenic total", "arsenic dissolved"))
asplot <- ggplot(site_as, aes(x = DateTime, y = Value, color = EMS_ID, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 25,linetype = "WQG Acute WL"), color = "red") +
  scale_linetype_manual(name = "T As", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = ("red")))) +
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Arsenic"~~(mu*g/L)) +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(asplot)
ggsave(filename = "T+D As.png", plot = asplot, h=4, w=6, units="in", dpi=300)

# plot T&D 
site_as <- filter(plotdata, Variable %in% c("arsenic total", "arsenic dissolved"))
asplot <- ggplot(site_as, aes(x = DateTime, y = Value, color = EMS_ID, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 25,linetype = "WQG Acute WL"), color = "red") +
  scale_linetype_manual(name = "T As", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Arsenic"~~(mu*g/L)) + coord_cartesian(ylim = c(0,130))+
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(asplot)
ggsave(filename = "T+D As2.png", plot = asplot, h=4, w=6, units="in", dpi=300)

# Total Arsenic facet

site_as <- filter(plotdata, Variable %in% "arsenic total")
asplot <- ggplot(site_as, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 25,linetype = "Acute WL"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = ("red")))) +
  theme_bw() +
  xlab("Date") +
  ylab("Total Arsenic"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(asplot)
ggsave(filename = "TAs_facet.png", plot = asplot, h=4, w=6, units="in", dpi=300)

# dissolved As facet

site_as <- filter(plotdata, Variable %in% "arsenic dissolved")
asplot <- ggplot(site_as, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Arsenic"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(asplot)
ggsave(filename = "DAs_facet.png", plot = asplot, h=4, w=6, units="in", dpi=300)

# dissolved Fe facet

site_fe <- filter(plotdata, Variable %in% "iron dissolved")
feplot <- ggplot(site_fe, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.3,linetype = "DW Aesthetic"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = c("red")))) +
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Iron (mg/L)") +  
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(feplot)
ggsave(filename = "DFe_facet.png", plot = feplot, h=4, w=6, units="in", dpi=300)


# Dissolved Hg facet

site_hg <- filter(plotdata, Variable %in% "mercury dissolved")
hgplot <- ggplot(site_hg, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) +
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Mercury"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(hgplot)
ggsave(filename = "Dhg_facet.png", plot = hgplot, h=4, w=6, units="in", dpi=300)

# Dissolved Mo
site_mo <- filter(plotdata, Variable %in% "molybdenum dissolved") 
moplot <- ggplot(site_mo, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.05,linetype = "Acute WL"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Molybdenum (mg/L)") +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(moplot)
ggsave(filename = "D Mo.png", plot = moplot, h=4, w=6, units="in", dpi=300)

# dissolved U facet

site_ur <- filter(plotdata, Variable %in% "uranium dissolved") 
urplot <- ggplot(site_ur, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Uranium"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(urplot)
ggsave(filename = "DUr_facet.png", plot = urplot, h=4, w=6, units="in", dpi=300)

# dissolved Se facet

site_se <- filter(plotdata, Variable %in% "selenium dissolved")
seplot <- ggplot(site_se, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 6,linetype = "Chronic WL"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Selenium"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(seplot)
ggsave(filename = "DSe_facet.png", plot = seplot, h=4, w=6, units="in", dpi=300)

# pH

site_ph <- filter(plotdata, Variable %in% "ph")
phplot <- ggplot(site_ph, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point(size=0.75) +
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("pH (pH unit)") + 
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(phplot)
ggsave(filename = "ph.png", plot = phplot, h=4, w=6, units="in", dpi=300)

# dissolved Al facet

site_al <- filter(plotdata, Variable %in% "aluminum dissolved")
alplot <- ggplot(site_al, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 5,linetype = "Chronic WL"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = ("red")))) +
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Aluminum (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(alplot)
ggsave(filename = "DAl_facet.png", plot = alplot, h=4, w=6, units="in", dpi=300)

# dissolved Co facet

site_co <- filter(plotdata, Variable %in% "cobalt dissolved")
Coplot <- ggplot(site_co, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 50,linetype = "Chronic IR"), color = "red") +
  scale_linetype_manual(name = "WWQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Cobalt"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(Coplot)
ggsave(filename = "DCo_facet.png", plot = Coplot, h=4, w=6, units="in", dpi=300)

# dissolved Ag facet

site_ag <- filter(plotdata, Variable %in% "silver dissolved")
agplot <- ggplot(site_ag, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Silver"~~(mu*g/L)) + coord_cartesian(ylim = c(0,0.1))+
  labs(shape="<MDL")+
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(agplot)
ggsave(filename = "DAg_facet.png", plot = agplot, h=4, w=6, units="in", dpi=300)

# Dissolved Mn facet

site_mn <- filter(plotdata, Variable %in% "manganese dissolved")
mnplot <- ggplot(site_mn, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Manganese (mg/L)") + 
  labs(shape="<MDL")+
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(mnplot)
ggsave(filename = "DMn_facet.png", plot = mnplot, h=4, w=6, units="in", dpi=300)

# Dissolved Pb facet

site_pb <- filter(plotdata, Variable %in% "lead dissolved")
pbplot <- ggplot(site_pb, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Lead"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(pbplot)
ggsave(filename = "DPb_facet.png", plot = pbplot, h=4, w=6, units="in", dpi=300)

# Dissolved Cd facet

site_cd <- filter(plotdata, Variable %in% "cadmium dissolved")
cdplot <- ggplot(site_cd, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Cadmium"~~(mu*g/L)) + coord_cartesian(ylim = c(0,0.25))+
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(cdplot)
ggsave(filename = "DCd_facet.png", plot = cdplot, h=4, w=6, units="in", dpi=300)

# Dissolved Ni facet

site_ni <- filter(plotdata, Variable %in% "nickel dissolved")
niplot <- ggplot(site_ni, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Nickel"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(niplot)
ggsave(filename = "DNi_facet.png", plot = niplot, h=4, w=6, units="in", dpi=300)

# Dissolved Zn facet

site_zn <- filter(plotdata, Variable %in% "zinc dissolved")
znplot <- ggplot(site_zn, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Zinc"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(znplot)
ggsave(filename = "DZn_facet.png", plot = znplot, h=4, w=6, units="in", dpi=300)

# Dissolved Sulphate

# look at hardness data
site_hard <- filter(plotdata, Variable %in% "hardcalc")
site_hard %>% group_by(EMS_ID) %>%
  summarise(min=min(Value), max=max(Value), mean=mean(Value)) %>% View(.)


# Dissolved SO42- facet

site_so4 <- filter(plotdata, Variable %in% "sulfate dissolved")
so4plot <- ggplot(site_so4, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Sulfate (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(so4plot)
ggsave(filename = "DSO4_facet.png", plot = so4plot, h=4, w=6, units="in", dpi=300)

# T NO2-
site_no <- filter(plotdata, Variable %in% "nitrite total")
noplot <- ggplot(site_no, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Nitrite (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(noplot)
ggsave(filename = "T NO2_facet.png", plot = noplot, h=4, w=6, units="in", dpi=300)

#D Nitrate
site_no <- filter(plotdata, Variable %in%  "nitrate dissolved")
noplot <- ggplot(site_no, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Dissolved Nitrate (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(noplot)
ggsave(filename = "T+D NO3.png", plot = noplot, h=4, w=6, units="in", dpi=300)

# D NO3 Facet
site_no <- filter(plotdata, Variable %in% "nitrate dissolved")
noplot <- ggplot(site_no, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Nitrate (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(noplot)
ggsave(filename = "D NO3_facet.png", plot = noplot, h=4, w=6, units="in", dpi=300)

# Specific Conductance
site_spc <- filter(plotdata, Variable %in% "specific conductance")
spcplot <- ggplot(site_spc, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  xlab("Date") +
  ylab("Specific Conductance"~~(mu*s/cm)) +
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(spcplot)
ggsave(filename = "specific conductance.png", plot = spcplot, h=4, w=6, units="in", dpi=300)

#Specific Conductance facet
site_spc <- filter(plotdata, Variable %in% "specific conductance")
spcplot <- ggplot(site_spc, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Specific Conductance"~~(mu*s/cm)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(spcplot)
ggsave(filename = "SPC_facet.png", plot = spcplot, h=4, w=6, units="in", dpi=300)

#Turbidity
site_ntu <- filter(plotdata, Variable %in% "turbidity")
ntuplot <- ggplot(site_ntu, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  xlab("Date") +
  ylab("Turbidity (NTU)") + 
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")
plot(ntuplot)
ggsave(filename = "turbidity.png", plot = ntuplot, h=4, w=6, units="in", dpi=300)

#facet for Turbidity

site_ntu <- filter(plotdata, Variable %in% "turbidity")
ntuplot <- ggplot(site_ntu, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Turbidity (NTU)") + coord_cartesian(ylim=c(0,20)) +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(ntuplot)
ggsave(filename = "turbidity_facet.png", plot = ntuplot, h=4, w=6, units="in", dpi=300)

# load Turbidity function and then run

ntu_WQO <- turbidity2_WQO(plotdata, "E215954", 10, 20, 50)

#write csv of turbidity threshold, data shows clear flows for threshold is 10 mg/L 
write.csv(ntu_WQO,'C:/R Projects/Similkameen-WQOs/data/report/SC_Plots/tables/SR_turbidity.csv', row.names = FALSE)

## plot turbidity against threshold for 0500075

ntuplot2 <- ggplot(site_ntu, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) +
  geom_line(data=ntu_WQO %>% select(-EMS_ID), aes(x = DateTime, y=Threshold), color="darkred") +
  theme_bw() +
  xlab("Date") +
  ylab("Turbidity (NTU)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(ntuplot2)
ggsave(filename = "tubidity_facetWQO.png", plot = ntuplot2, h=4, w=6, units="in", dpi=300)

# plot NH4

plotnames <- c('nitrogen ammonia total' = "T-NH4",
               'nitrogen ammonia dissolved' = "D-NH4")

site_NH4 <- filter(plotdata, Variable %in% c("nitrogen ammonia total",
                                             "nitrogen ammonia dissolved"))
NH4plot <- ggplot(site_NH4, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Ammonia (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(NH4plot)
ggsave(filename = "T+D NH4.png", plot = NH4plot, h=4, w=6, units="in", dpi=300)

# T NH4
site_NH4 <- filter(plotdata, Variable %in% c("nitrogen ammonia total"))
NH4plot <- ggplot(site_NH4, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Ammonia (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(NH4plot)
ggsave(filename = "T NH4_facet.png", plot = NH4plot, h=4, w=6, units="in", dpi=300)

# D NH4
site_NH4 <- filter(plotdata, Variable %in% "nitrogen ammonia dissolved")
NH4plot <- ggplot(site_NH4, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Ammonia (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(NH4plot)
ggsave(filename = "D NH4_facet.png", plot = NH4plot, h=4, w=6, units="in", dpi=300)

# plot Copper

plotnames <- c('copper total' = "T-Cu",
               'copper dissolved' = "D-Cu")

site_Cu <- filter(plotdata, Variable %in% c("copper total",
                                            "copper dissolved"))
Cuplot <- ggplot(site_Cu, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Copper"~~(mu*g/L)) + coord_cartesian(ylim = c(0,60)) +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(Cuplot)
ggsave(filename = "T+D Cu.png", plot = Cuplot, h=4, w=6, units="in", dpi=300)

# T Cu
site_Cu <- filter(plotdata, Variable %in% c("copper total"))
Cuplot <- ggplot(site_Cu, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Copper"~~(mu*g/L)) + coord_cartesian(ylim = c(0,60)) +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(Cuplot)
ggsave(filename = "T Cu_facet.png", plot = Cuplot, h=4, w=6, units="in", dpi=300)

# D Cu
site_Cu <- filter(plotdata, Variable %in% "copper dissolved")
Cuplot <- ggplot(site_Cu, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Copper"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(Cuplot)
ggsave(filename = "D Cu_facet.png", plot = Cuplot, h=4, w=6, units="in", dpi=300)
