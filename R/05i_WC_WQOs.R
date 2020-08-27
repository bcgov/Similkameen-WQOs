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


# This script will graph the parameters for the Similkameen River, plot WQGs/WQOs

## Load packages 

library(tidyverse)
library(lubridate)
library(magrittr)
library(plotly)


setwd('C:/R Projects/Similkameen-WQOs/')

# Load data that is ready for plotting with WQOs
plotdata <- read_csv("data/report/WC_Plots/tables/WC_sites2.csv")

#no WQGs calculated for NH4


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
                          levels=c( "E307368", "E266462", "E287173", "E254310", 
                                    "0500397", "E254309", "0500101", "E254311",
                                    "E287172", "E318572"))

units <- distinct(plotdata, Units)

# set working drive for plots
setwd('C:/R Projects/Similkameen-WQOs/data/report/WC_Plots/WQOs')

## PLOTTING

#e.coli
site_ecoli <- filter(plotdata, Variable %in% "e coli")
ecoliplot <- ggplot(site_ecoli, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  xlab("Date") +
  ylab("E. Coli. (cfu/100mL)") + coord_cartesian(ylim = c(0,45)) +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape = "<MDL")
plot(ecoliplot)
ggsave(filename = "e.coli.png", plot = ecoliplot, h=4, w=6, units="in", dpi=300)

# Total Alkalinity 4.5
site_alk <- filter(plotdata, Variable %in% "alkalinity total 4.5")
alkplot <- ggplot(site_alk, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  xlab("Date") +
  ylab("Total Alkalinity 4.5 (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(alkplot)
ggsave(filename = "alkalinity total 4.5.png", plot = alkplot, h=4, w=6, units="in", dpi=300)

#Alkalinity Facet
site_alk <- filter(plotdata, Variable %in% "alkalinity total 4.5")
alkplot <- ggplot(site_alk, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Alkalinity 4.5 (mg/L)") +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(alkplot)
ggsave(filename = "alkalinity total 4.5_facet.png", plot = alkplot, h=4, w=6, units="in", dpi=300)


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
  ylab("TSS (mg/L)") +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(tssplot)
ggsave(filename = "tss_facet.png", plot = tssplot, h=4, w=6, units="in", dpi=300)

# load TSS function and then run

(tsslimits<-TSS_WQO(plotdata, "E307368", 10, 10, 100))

#write csv of TSS threshold, data shows clear flows for threshold is 10 mg/L 
write.csv(tsslimits,'C:/R Projects/Similkameen-WQOs/data/report/WC_Plots/tables/SR_tss.csv', row.names = FALSE)

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


##need to rearrange tsslimits so threshold is in EMS_Id column
#first remove columns
tsslimits <- subset(tsslimits, select = -c(Code, ResultLetter, Station))
#make dataframe of WQO
tssWQO2 <- tsslimits %>% dplyr::select(DateTime, Variable, Units, EMS_ID, CENSOR, Threshold)
#rename Threshold to Value
tssWQO2 <- tssWQO2 %>% rename(Value = Threshold)
#remane tss to WQO
tssWQO2$Variable <- plyr::revalue (tssWQO2$Variable, c("tss" = "WQO"))
#combine tsslmits and tssWQO2
tsslimits2 <- bind_rows(tsslimits, tssWQO2, id=NULL)
#remove threshold column
tsslimits2 <- subset(tsslimits2, select = -Threshold)

## plot tss against threshold for E307368

tssplot2 <- ggplot(tsslimits2, aes(x = DateTime, y = Value, col = Variable, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Concentration"~~(mu*g/L)) + 
  labs(col = "E307368", shape="<MDL")
plot(tssplot2)
ggsave(filename = "TSS_WQO E307368.png", plot = tssplot2, h=4, w=6, units="in", dpi=300)


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

#Total Phosphorus

site_P <- filter(plotdata, Variable %in% "phosphorus total")
Pplot <- ggplot(site_P, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key= element_rect(fill="NA")) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("Total Phosphorus (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")
plot(Pplot)
ggsave(filename = "TP.png", plot = Pplot, h=4, w=6, units="in", dpi=300)

#Facet for Total Phosphorus

site_P <- filter(plotdata, Variable %in% "phosphorus total")
Pplot <- ggplot(site_P, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Phosphorus (mg/L)") +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(Pplot)
ggsave(filename = "TP_facet.png", plot = Pplot, h=4, w=6, units="in", dpi=300)

#Total Dissolved Phosphorus

site_P <- filter(plotdata, Variable %in% "phosphorus total dissolved")
Pplot <- ggplot(site_P, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key= element_rect(fill="NA")) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("Total Dissolved Phosphorus (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(Pplot)
ggsave(filename = "DP.png", plot = Pplot, h=4, w=6, units="in", dpi=300)

# Total Dissolved Phosphorus Facet

site_P <- filter(plotdata, Variable %in% "phosphorus total dissolved")
Pplot <- ggplot(site_P, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Dissolved Phosphorus (mg/L)") +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(Pplot)
ggsave(filename = "DP_facet.png", plot = Pplot, h=4, w=6, units="in", dpi=300)


#Total and Dissolved Chromium

plotnames <- c('chromium total' = "T-Cr", 'chromium dissolved' = "D-Cr")

# Plot T&D Cr facet 
site_cr <- filter(plotdata, Variable %in% c("chromium total", "chromium dissolved"))
crplot <- ggplot(site_cr, aes(x = DateTime, y = Value, color = EMS_ID, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 8.9,linetype = "AL (III)"), color = "blue") +
  geom_hline(aes(yintercept = 1,linetype = "AL (VI)"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("blue", "red")))) +
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Chromium"~~(mu*g/L)) + 
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(crplot)
ggsave(filename = "T+D Cr.png", plot = crplot, h=4, w=6, units="in", dpi=300)

# total Cr facet

site_Cr <- filter(plotdata, Variable %in% "chromium total")
Crplot <- ggplot(site_Cr, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 8.9,linetype = "AL (III)"), color = "blue") +
  geom_hline(aes(yintercept = 1,linetype = "AL (VI)"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("blue", "red")))) +
  theme_bw() +
  xlab("Date") +
  ylab("Total Chromium"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(Crplot)
ggsave(filename = "TCr_facet.png", plot = Crplot, h=4, w=6, units="in", dpi=300)

# dissolved Cr facet

site_Cr <- filter(plotdata, Variable %in% "chromium dissolved")
Crplot <- ggplot(site_Cr, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Chromium"~~(mu*g/L)) +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(Crplot)
ggsave(filename = "DCr_facet.png", plot = Crplot, h=4, w=6, units="in", dpi=300)

#TDS

site_tds <- filter(plotdata, Variable %in% c("tds"))
tdsplot <- ggplot(site_tds, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point(size=0.75) + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("Total Dissolved Solids (mg/L)") + 
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL") 
plot(tdsplot)
ggsave(filename = "TDS.png", plot = tdsplot, h=4, w=6, units="in", dpi=300)

# TDS facet

site_tds <- filter(plotdata, Variable %in% "tds")
tdsplot <- ggplot(site_tds, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Dissolved Soilds (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(tdsplot)
ggsave(filename = "TDS_facet.png", plot = tdsplot, h=4, w=6, units="in", dpi=300)


# Total and Dissolved Arsenic

plotnames <- c('arsenic total' = "T-As", 'arsenic dissolved' = "D-As")

site_as <- filter(plotdata, Variable %in% c("arsenic total", "arsenic dissolved"))
asplot <- ggplot(site_as, aes(x = DateTime, y = Value, color = EMS_ID, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 5,linetype = "WQG Acute AL"), color = "red") +
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

# plot T&D without WQG
site_as <- filter(plotdata, Variable %in% c("arsenic total", "arsenic dissolved"))
asplot <- ggplot(site_as, aes(x = DateTime, y = Value, color = EMS_ID, shape = CENSOR)) +
  geom_point(size=0.75) + 
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
ggsave(filename = "T+D As2.png", plot = asplot, h=4, w=6, units="in", dpi=300)

# Total Arsenic facet

site_as <- filter(plotdata, Variable %in% "arsenic total")
asplot <- ggplot(site_as, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
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

# Total and Dissolved Iron

plotnames <- c('iron total' = "T-Fe", 'iron dissolved' = "D-Fe")

site_fe <- filter(plotdata, Variable %in% c("iron total", "iron dissolved"))
feplot <- ggplot(site_fe, aes(x = DateTime, y = Value, color = EMS_ID, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.35,linetype = "Acute AL D Fe"), color = "blue") +
  geom_hline(aes(yintercept = 0.3,linetype = "DW T Fe"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("blue", "red")))) +
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Iron (mg/L)") + 
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(feplot)
ggsave(filename = "T+D Fe.png", plot = feplot, h=4, w=6, units="in", dpi=300)
#ggplotly(feplot) #this allows you to zoom in and see info ono each plot point

# Total Fe facet

site_fe <- filter(plotdata, Variable %in% "iron total")
feplot <- ggplot(site_fe, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.3,linetype = "DW"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = c("red")))) +
  theme_bw() +
  xlab("Date") +
  ylab("Total Iron (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+  
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(feplot)
ggsave(filename = "TFe_facet.png", plot = feplot, h=4, w=6, units="in", dpi=300)

# dissolved Fe facet

site_fe <- filter(plotdata, Variable %in% "iron dissolved")
feplot <- ggplot(site_fe, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.35,linetype = "Acute AL"), color = "red") +
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


# Total Hg facet

site_hg <- filter(plotdata, Variable %in% "mercury total")
hgplot <- ggplot(site_hg, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 1,linetype = "Acute AL"), color = "blue") +
  geom_hline(aes(yintercept = 0.02,linetype = "Chronic AL"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("blue", "red")))) +
  theme_bw() +
  xlab("Date") +
  ylab("Total Mercury"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(hgplot)
ggsave(filename = "Thg_facet.png", plot = hgplot, h=4, w=6, units="in", dpi=300)

site_hg <- filter(plotdata, Variable %in% "mercury total")
hgplot <- ggplot(site_hg, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.02,linetype = "Chronic AL"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = ("red")))) +
  theme_bw() +
  xlab("Date") +
  ylab("Total Mercury"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(hgplot)
ggsave(filename = "Thg_facet2.png", plot = hgplot, h=4, w=6, units="in", dpi=300)

# Total and Dissolved Molybdenum

plotnames <- c('molybdenum total' = "T-Mo", 'molybdenum dissolved' = "D-Mo")

site_mo <- filter(plotdata, Variable %in% c("molybdenum total", "molybdenum dissolved"))
moplot <- ggplot(site_mo, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.01,linetype = "Chronic IR"), color = "red") +
  scale_linetype_manual(name = "WQG T Mo", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Total Molybdenum (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(moplot)
ggsave(filename = "T&D Mo.png", plot = moplot, h=4, w=6, units="in", dpi=300)

#T Mo

site_mo <- filter(plotdata, Variable %in% "molybdenum total") 
moplot <- ggplot(site_mo, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.01,linetype = "Chronic IR"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_bw() +
  xlab("Date") +
  ylab("Total Molybdenum (mg/L)") +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(moplot)
ggsave(filename = "T Mo.png", plot = moplot, h=4, w=6, units="in", dpi=300)

# Dissolved Mo
site_mo <- filter(plotdata, Variable %in% "molybdenum dissolved") 
moplot <- ggplot(site_mo, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Molybdenum (mg/L)") +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(moplot)
ggsave(filename = "D Mo.png", plot = moplot, h=4, w=6, units="in", dpi=300)

# Total and Dissolved Uranium

plotnames <- c('uranium total' = "T-U", 'uranium dissolved' = "D-U")

site_ur <- filter(plotdata, Variable %in% c("uranium total", "uranium dissolved" ))
urplot <- ggplot(site_ur, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 10,linetype = "WWQG Chronic IR"), color = "blue") +
  geom_hline(aes(yintercept = 8.5,linetype = "WWQG Chronic AL"), color = "red") +
  scale_linetype_manual(name = "T-U", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("red", "blue")))) +
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Uranium"~~(mu*g/L)) +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(urplot)
ggsave(filename = "T+D Ur.png", plot = urplot, h=4, w=6, units="in", dpi=300)

# Total U facet

site_ur <- filter(plotdata, Variable %in% "uranium total") 
urplot <- ggplot(site_ur, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Uranium"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(urplot)
ggsave(filename = "TUr_facet.png", plot = urplot, h=4, w=6, units="in", dpi=300)

# dissolved U facet

site_ur <- filter(plotdata, Variable %in% "uranium dissolved") 
urplot <- ggplot(site_ur, aes(x = DateTime, y = Value, shape=CENSOR)) +
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

# Total and Dissolved Selenium

plotnames <- c('selenium total' = "T-Se", 'selenium dissolved' = "D-Se")

site_se <- filter(plotdata, Variable %in% c("selenium total", "selenium dissolved" ))
seplot <- ggplot(site_se, aes(x = DateTime, y = Value, color = EMS_ID, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 2,linetype = "AL"), color = "red") +
  scale_linetype_manual(name = "WQG T-Se", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Selenium"~~(mu*g/L)) +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape = "<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(seplot)
ggsave(filename = "T+D Se.png", plot = seplot, h=4, w=6, units="in", dpi=300)

# Total Se facet

site_se <- filter(plotdata, Variable %in% "selenium total")
seplot <- ggplot(site_se, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 2,linetype = "AL"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_bw() +
  xlab("Date") +
  ylab("Total Selenium"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(seplot)
ggsave(filename = "TSe_facet.png", plot = seplot, h=4, w=6, units="in", dpi=300)

# dissolved Se facet

site_se <- filter(plotdata, Variable %in% "selenium dissolved")
seplot <- ggplot(site_se, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
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

# pH field

site_ph <- filter(plotdata, Variable %in% "ph-field")
phplot <- ggplot(site_ph, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 8.5,linetype = "Max"), color = "red") +
  geom_hline(aes(yintercept = 6.5,linetype = "Min"), color = "red") +
  scale_linetype_manual(name = "WQO", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("red", "red")))) +
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("Field pH (pH unit") + 
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(phplot)
ggsave(filename = "ph field.png", plot = phplot, h=4, w=6, units="in", dpi=300)

# pH
#remove super low pH
plotdata <- plotdata %>% dplyr::filter(!((EMS_ID=="E307368" & 
                                            Value == 2.42)))

site_ph <- filter(plotdata, Variable %in% "ph")
phplot <- ggplot(site_ph, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 8.5,linetype = "Max"), color = "red") +
  geom_hline(aes(yintercept = 6.5,linetype = "Min"), color = "red") +
  scale_linetype_manual(name = "WQO", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("red", "red")))) +
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("pH (pH unit)") + 
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(phplot)
ggsave(filename = "ph.png", plot = phplot, h=4, w=6, units="in", dpi=300)

# Dissolved oxygen

site_do<- filter(plotdata, Variable %in% "oxygen dissolved")
doplot <- ggplot(site_do, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point(size=0.75) + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("Dissolved Oxygen (mg/L)") + 
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(doplot)
ggsave(filename = "DO.png", plot = doplot, h=4, w=6, units="in", dpi=300)

# Total and Dissovled Aluminum

plotnames <- c('aluminum total' = "T-Al", 'aluminum dissolved' = "D-Al")

site_al <- filter(plotdata, Variable %in% c("aluminum total", "aluminum dissolved" ))
alplot <- ggplot(site_al, aes(x = DateTime, y = Value, color = EMS_ID, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.1,linetype = "Acute AL"), color = "blue") +
  geom_hline(aes(yintercept = 0.05,linetype = "Chronic AL"), color = "red") +
  scale_linetype_manual(name = "WQG D-Al", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("blue", "red")))) +
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Aluminum (mg/L)") + 
  theme(text=element_text(size=8)) +
  labs(color="Station", shape = "<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(alplot)
ggsave(filename = "T+D Al.png", plot = alplot, h=4, w=6, units="in", dpi=300)

# Total Al facet

site_al <- filter(plotdata, Variable %in% "aluminum total")
alplot <- ggplot(site_al, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 5,linetype = "Acute WL/LS/IR"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = ("red")))) +
  theme_bw() +
  xlab("Date") +
  ylab("Total Aluminum (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(alplot)
ggsave(filename = "TAl_facet.png", plot = alplot, h=4, w=6, units="in", dpi=300)

# dissolved Al facet

site_al <- filter(plotdata, Variable %in% "aluminum dissolved")
alplot <- ggplot(site_al, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.1,linetype = "Acute AL"), color = "blue") +
  geom_hline(aes(yintercept = 0.05,linetype = "Chronic AL"), color = "red") +
  scale_linetype_manual(name = "WQO/WQG", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("blue", "red")))) +
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

# Plot T&D Co facet 

plotnames <- c('cobalt total' = "T-Co", 'cobalt dissolved' = "D-Co")

site_co <- filter(plotdata, Variable %in% c("cobalt total", "cobalt dissolved"))
coplot <- ggplot(site_co, aes(x = DateTime, y = Value, color = EMS_ID, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 4,linetype = "Chronic AL"), color = "red") +
  scale_linetype_manual(name = "T Co WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Cobalt"~~(mu*g/L)) +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(coplot)
ggsave(filename = "T+D Co.png", plot = coplot, h=4, w=6, units="in", dpi=300)

# total Co facet

site_co <- filter(plotdata, Variable %in% "cobalt total")
Coplot <- ggplot(site_co, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Cobalt"~~(mu*g/L)) + coord_cartesian(ylim = c(0,3))+
                                              
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(Coplot)
ggsave(filename = "TCo_facet.png", plot = Coplot, h=4, w=6, units="in", dpi=300)

# dissolved Co facet

site_co <- filter(plotdata, Variable %in% "cobalt dissolved")
Coplot <- ggplot(site_co, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
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

# Total and Dissovled Silver
# look at hardness data
site_hard <- filter(plotdata, Variable %in% "hardcalc")
site_hard %>% group_by(EMS_ID) %>%
  summarise(min=min(Value), max=max(Value), mean=mean(Value)) %>% View(.)
#guideline based on hardness <100 which is what the reference station is

plotnames <- c('silver total' = "T-Ag", 'silver dissolved' = "D-Ag")

site_ag <- filter(plotdata, Variable %in% c("silver total", "silver dissolved" ))
agplot <- ggplot(site_ag, aes(x = DateTime, y = Value, color = EMS_ID, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.1,linetype = "Acute AL"), color = "blue") +
  geom_hline(aes(yintercept = 0.05,linetype = "Chronic AL"), color = "red") +
  scale_linetype_manual(name = "WQG T-Ag", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("blue", "red")))) +
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Silver"~~(mu*g/L)) +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape = "<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(agplot)
ggsave(filename = "T+D Ag.png", plot = agplot, h=4, w=6, units="in", dpi=300)

# Total Ag facet

site_ag <- filter(plotdata, Variable %in% "silver total")
agplot <- ggplot(site_ag, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.1,linetype = "Acute AL"), color = "blue") +
  geom_hline(aes(yintercept = 0.05,linetype = "Chronic AL"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("blue", "red")))) +
  theme_bw() +
  xlab("Date") +
  ylab("Total Silver"~~(mu*g/L)) +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) + 
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(agplot)
ggsave(filename = "TAg_facet.png", plot = agplot, h=4, w=6, units="in", dpi=300)

# dissolved Ag facet

site_ag <- filter(plotdata, Variable %in% "silver dissolved")
agplot <- ggplot(site_ag, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.1,linetype = "Acute AL"), color = "blue") +
  geom_hline(aes(yintercept = 0.05,linetype = "Chronic AL"), color = "red") +
  scale_linetype_manual(name = "WQG T-Ag", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("blue", "red")))) +
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Silver"~~(mu*g/L)) + 
  labs(shape="<MDL")+
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(agplot)
ggsave(filename = "DAg_facet.png", plot = agplot, h=4, w=6, units="in", dpi=300)

# Total and Dissolved Manganese

plotnames <- c('manganese total' = "T-Mn", 'manganese dissolved' = "D-Mn")

site_mn <- filter(plotdata, Variable %in% c("manganese total", "manganese dissolved"))
mnplot <- ggplot(site_mn, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Managanese (mg/L)") + 
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(mnplot)
ggsave(filename = "T+D Mn.png", plot = mnplot, h=4, w=6, units="in", dpi=300)

# Total Mn facet

site_mn <- filter(plotdata, Variable %in% "manganese total")
mnplot <- ggplot(site_mn, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Manganese (mg/L)") + coord_cartesian(ylim=c(0,0.6)) +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(mnplot)
ggsave(filename = "TMn_facet.png", plot = mnplot, h=4, w=6, units="in", dpi=300)

# Dissolved Mn facet

site_mn <- filter(plotdata, Variable %in% "manganese dissolved")
mnplot <- ggplot(site_mn, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Manganese (mg/L)") + 
  labs(shaoe="<MDL")+
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(mnplot)
ggsave(filename = "DMn_facet.png", plot = mnplot, h=4, w=6, units="in", dpi=300)

#Calculate AL WQGs using hardness
site_hard <- filter(plotdata, Variable %in% "hardcalc")
AcuteAL<- site_hard %>%
  mutate(Acute_Al = Value*0.01102+0.54)
#remove columns
AcuteAL <- dplyr::select(AcuteAL, -Variable, -Code, -Value)
#add columns for guideline to be in Variable
AcuteAL[c("Variable")] <- NA
AcuteAL$Variable[is.na(AcuteAL$Variable)] <- "AcuteAL"
#rename AcuteAL column to Value
AcuteAL <- AcuteAL %>% rename(Value = Acute_Al)

ChronicAL <- site_hard %>%
  mutate(Chronic_Al = Value*0.0044+0.605)
#remove columns
ChronicAL <- dplyr::select(ChronicAL, -Variable, -Code, -Value)
#add columns for guideline to be in Variable
ChronicAL[c("Variable")] <- NA
ChronicAL$Variable[is.na(ChronicAL$Variable)] <- "ChronicAL"
#rename AcuteAL column to Value
ChronicAL <- ChronicAL %>% rename(Value = Chronic_Al)

#merge WQG tables
WQGs <- bind_rows(AcuteAL, ChronicAL, id=NULL)
#make df of T&D MN
site_mn2 <- filter(plotdata, Variable %in% c("manganese total", "manganese dissolved"))
#merge WQGs with site_mn
site_mn2 <- bind_rows(WQGs, site_mn2)

#T&D Mn E307368 w/WQGs

site_mn3 <- filter(site_mn2, Variable %in% c("manganese total", 
                                             "manganese dissolved",
                                             "AcuteAL", "ChronicAL") 
                   & EMS_ID == "E307368")
mnplot2 <- ggplot(site_mn3, aes(x = DateTime, y = Value, col = Variable)) +
  geom_point(size=0.75) + 
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Concentration (mg/L)") + coord_cartesian(ylim=c(0,2.25)) +
  labs(col = "E307368")
plot(mnplot2)
ggsave(filename = "T+D MnE307368.png", plot = mnplot2, h=4, w=6, units="in", dpi=300)

# Total and dissolved lead

plotnames <- c('lead total' = "T-Pb", 'lead dissolved' = "D-Pb")

site_pb <- filter(plotdata, Variable %in% c("lead total", "lead dissolved"))
pbplot <- ggplot(site_pb, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Lead"~~(mu*g/L)) +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(pbplot)
ggsave(filename = "T+D Pb.png", plot = pbplot, h=4, w=6, units="in", dpi=300)

# Total Pb facet

site_pb <- filter(plotdata, Variable %in% "lead total")
pbplot <- ggplot(site_pb, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Lead"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(pbplot)
ggsave(filename = "TPb_facet.png", plot = pbplot, h=4, w=6, units="in", dpi=300)

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

#Calculate WQOs using hardness
site_hard2 <- filter(plotdata, Variable %in% "hardcalc")
ChronicAL<- site_hard2 %>%
  mutate(Value = exp(1.273 * log(Value) - 4.704) + 3.31)
#rename column and some variables in df
ChronicAL$Variable <- gsub("hardcalc", "ChronicAL", ChronicAL$Variable)
ChronicAL$Code <- gsub("H--C", "WQO1", ChronicAL$Code)

AcuteAL <- site_hard2 %>%
  mutate(Value = exp(1.273 * log(Value) - 1.460))
#rename column and some variables in df
AcuteAL$Variable <- gsub("hardcalc", "AcuteAL", AcuteAL$Variable)
AcuteAL$Code <- gsub("H--C", "WQO2", AcuteAL$Code)

#merge WQG tables
WQGs <- bind_rows(AcuteAL, ChronicAL, id=NULL)
#make df of T&D lead
site_pb2 <- filter(plotdata, Variable %in% c("lead total", "lead dissolved"))
#merge WQGs with site_pb
site_pb2 <- bind_rows(WQGs, site_pb2, id=NULL)

#T&D Pb E307368 w/WQGs

site_pb3 <- filter(site_pb2, Variable %in% c("lead total", 
                                             "lead dissolved",
                                             "AcuteAL", "ChronicAL") 
                   & EMS_ID == "E307368")
pbplot2 <- ggplot(site_pb3, aes(x = DateTime, y = Value, col = Variable, shape=CENSOR)) +
  geom_point(size=0.75)  +
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Concentration"~~(mu*g/L)) + coord_cartesian(ylim=c(0,130)) +
  labs(col = "E307368", shape= "<MDL")
plot(pbplot2)
ggsave(filename = "T+D pbE307368.png", plot = pbplot2, h=4, w=6, units="in", dpi=300)


# Total and dissolved cadmium

plotnames <- c('cadmium total' = "T-Cd", 'cadmium dissolved' = "D-Cd")

site_cd <- filter(plotdata, Variable %in% c("cadmium total", "cadmium dissolved"))
cdplot <- ggplot(site_cd, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Cadmium"~~(mu*g/L)) +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(cdplot)
ggsave(filename = "T+D Cd.png", plot = cdplot, h=4, w=6, units="in", dpi=300)

# Total Cd facet

site_cd <- filter(plotdata, Variable %in% "cadmium total")
cdplot <- ggplot(site_cd, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Cadmium"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(cdplot)
ggsave(filename = "TCd_facet.png", plot = cdplot, h=4, w=6, units="in", dpi=300)

# Dissolved Cd facet

site_cd <- filter(plotdata, Variable %in% "cadmium dissolved")
cdplot <- ggplot(site_cd, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Cadmium"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(cdplot)
ggsave(filename = "DCd_facet.png", plot = cdplot, h=4, w=6, units="in", dpi=300)

#Calculate WQOs using hardness
site_hard3 <- filter(plotdata, Variable %in% "hardcalc")
ChronicAL<- site_hard3 %>%
  mutate(Value = exp(0.736 * log(Value) - 4.943))
#rename column and some variables in df
ChronicAL$Variable <- gsub("hardcalc", "ChronicAL", ChronicAL$Variable)
ChronicAL$Code <- gsub("H--C", "WQO1", ChronicAL$Code)

AcuteAL <- site_hard3 %>%
  mutate(Value = exp(1.03 * log(Value) - 5.274))
#rename column and some variables in df
AcuteAL$Variable <- gsub("hardcalc", "AcuteAL", AcuteAL$Variable)
AcuteAL$Code <- gsub("H--C", "WQO2", AcuteAL$Code)

#merge WQG tables
WQGs <- bind_rows(AcuteAL, ChronicAL, id=NULL)
#merge WQGs with site_cd
site_cd <- bind_rows(WQGs, site_cd)


#T&D Cd E307368 w/WQGs

site_cd2 <- filter(site_cd, Variable %in% c("AcuteAL","ChronicAL",
                                            "cadmium dissolved")
                   & EMS_ID == "E307368")           


#plot data in specific order
site_cd2$Variable <- factor(site_cd2$Variable, 
                            levels=c("AcuteAL", "ChronicAL", 
                                     "cadmium dissolved"))


cdplot2 <- ggplot(site_cd2, aes(x = DateTime, y = Value, col = Variable, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Concentration"~~(mu*g/L)) + coord_cartesian(ylim=c(0,0.8)) +
  labs(col = "E307368", shape="<MDL")
plot(cdplot2)
ggsave(filename = "T+D cdE307368.png", plot = cdplot2, h=4, w=6, units="in", dpi=300)


# Total and dissolved nickel

plotnames <- c('nickel total' = "T-Ni", 'nickel dissolved' = "D-Ni")

site_ni <- filter(plotdata, Variable %in% c("nickel total", "nickel dissolved"))
niplot <- ggplot(site_ni, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Nickel"~~(mu*g/L)) +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(niplot)
ggsave(filename = "T+D Ni.png", plot = niplot, h=4, w=6, units="in", dpi=300)

# Total Ni facet

site_ni <- filter(plotdata, Variable %in% "nickel total")
niplot <- ggplot(site_ni, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Nickel"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(niplot)
ggsave(filename = "TNi_facet.png", plot = niplot, h=4, w=6, units="in", dpi=300)

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

# Total and dissolved Zinc

plotnames <- c('zinc total' = "T-Zn", 'zinc dissolved' = "D-Zn")

site_zn <- filter(plotdata, Variable %in% c("zinc total", "zinc dissolved"))
znplot <- ggplot(site_zn, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 40,linetype = "Acute AL"), color = "blue") +
  geom_hline(aes(yintercept = 15,linetype = "Chronic AL"), color = "red") +
  scale_linetype_manual(name = "WQG T-Zn", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("blue", "red")))) +
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Zinc"~~(mu*g/L)) +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2, labeller = as_labeller(plotnames))
plot(znplot)
ggsave(filename = "T+D Zn.png", plot = znplot, h=4, w=6, units="in", dpi=300)

# Total zn facet

site_zn <- filter(plotdata, Variable %in% "zinc total")
znplot <- ggplot(site_zn, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Zinc"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(znplot)
ggsave(filename = "TZn_facet.png", plot = znplot, h=4, w=6, units="in", dpi=300)

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
  geom_hline(aes(yintercept = 309,linetype = "AL (hardness 76-180)"), color = "blue") +
  geom_hline(aes(yintercept = 218,linetype = "AL (hardness 31-75)"), color = "red") +
  scale_linetype_manual(name = "WQG Chronic", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("red", "blue")))) +
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

# D NO2-
site_no <- filter(plotdata, Variable %in% "nitrite dissolved")
noplot <- ggplot(site_no, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Nitrite (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(noplot)
ggsave(filename = "D NO2_facet.png", plot = noplot, h=4, w=6, units="in", dpi=300)

# D NO3-
site_no <- filter(plotdata, Variable %in% "nitrate dissolved")
noplot <- ggplot(site_no, aes(x = DateTime, y = Value, color=EMS_ID, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 3,linetype = "Chronic AL"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_classic() +
  xlab("Date") +
  ylab("Dissolved Nitrate (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")
plot(noplot)
ggsave(filename = "D NO3.png", plot = noplot, h=4, w=6, units="in", dpi=300)

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

# Specific Conductance Field
site_spc <- filter(plotdata, Variable %in% "specific conductivity-field")
spcplot <- ggplot(site_spc, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  xlab("Date") +
  ylab("Specific Conductivity-Field"~~(mu*s/cm)) +
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(spcplot)
ggsave(filename = "Cond-field.png", plot = spcplot, h=4, w=6, units="in", dpi=300)

# Temperature
site_t <- filter(plotdata, Variable %in% "temperature-field")
tplot <- ggplot(site_t, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  xlab("Date") +
  ylab("Field Temperature (°C)") +
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(tplot)
ggsave(filename = "field temp.png", plot = tplot, h=4, w=6, units="in", dpi=300)

# Temperature
site_t <- filter(plotdata, Variable %in% "temperature")
tplot <- ggplot(site_t, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  xlab("Date") +
  ylab("Temperature (°C)") +
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(tplot)
ggsave(filename = "temp.png", plot = tplot, h=4, w=6, units="in", dpi=300)


# Dissolved Organic Carbon
site_doc <- filter(plotdata, Variable %in% "carbon dissolved organic")
docplot <- ggplot(site_doc, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point(size=0.75) + 
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Dissolved Organic Carbon (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2)
plot(docplot)
ggsave(filename = "DOC.png", plot = docplot, h=4, w=6, units="in", dpi=300)

# DOC facet
site_doc <- filter(plotdata, Variable %in% "carbon dissolved organic")
docplot <- ggplot(site_doc, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Organic Carbon (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(docplot)
ggsave(filename = "DOC_facet.png", plot = docplot, h=4, w=6, units="in", dpi=300)

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
  ylab("Turbidity (NTU)") + coord_cartesian(ylim=c(0,60)) +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(ntuplot)
ggsave(filename = "turbidity_facet.png", plot = ntuplot, h=4, w=6, units="in", dpi=300)

# load Turbidity function and then run

ntu_WQO <- turbidity_WQO(plotdata, "E307368", 1, 5, 10, 5, 50)

#write csv of turbidity threshold, data shows clear flows for threshold is 10 mg/L 
write.csv(ntu_WQO,'C:/R Projects/Similkameen-WQOs/data/report/WC_Plots/tables/SR_turbidity.csv', row.names = FALSE)

## plot turbidity against threshold for 0500075

ntuplot2 <- ggplot(site_ntu, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) +
  geom_line(data=ntu_WQO %>% select(-EMS_ID), aes(x = DateTime, y=Threshold), color="darkred") +
  theme_bw() +
  xlab("Date") +
  ylab("Turbidity (NTU)") + coord_cartesian(ylim = c(0,100))+
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(ntuplot2)
ggsave(filename = "tubidity_facetWQO.png", plot = ntuplot2, h=4, w=6, units="in", dpi=300)

##need to rearrange tubidity_WQO so threshold is in EMS_Id column
#first remove columns
tubidity_WQO <- subset(ntu_WQO, select = -c(Code, ResultLetter, Station, CENSOR))
#make dataframe of WQO
WQO2 <- tubidity_WQO %>% dplyr::select(DateTime, Variable, Units, EMS_ID, Threshold)
#rename Threshold to Value
WQO2 <- WQO2 %>% rename(Value = Threshold)
#remane turbidity to WQO
WQO2$Variable <- plyr::revalue (WQO2$Variable, c("turbidity" = "WQO"))
#combine tsslmits and tssWQO2
ntu_WQO <- bind_rows(tubidity_WQO, WQO2, id=NULL)
#remove threshold column
ntu_WQO <- subset(ntu_WQO, select = -Threshold)

## plot turbidity against threshold for E307368

ntuplot2 <- ggplot(ntu_WQO, aes(x = DateTime, y = Value, col = Variable)) +
  geom_point(size=0.75) + 
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("Concentration"~~(mu*g/L)) + coord_cartesian(ylim = c(0,3)) +
  labs(col = "E307368", shape="<MDL")
plot(ntuplot2)
ggsave(filename = "Turbidity_WQO E307368.png", plot = ntuplot2, h=4, w=6, units="in", dpi=300)

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

# no NH4WQG data


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
Cuplot <- ggplot(site_Cu, aes(x = DateTime, y = Value)) +
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

# Facet of Cu with guidelines 

## make dataframe of Wolfe Creek sites
Cu_WC <- filter(CuWQGs, EMS_ID == "E307368" | EMS_ID == "E266462" | EMS_ID == "E287173" |
                  EMS_ID == "E254310" | EMS_ID ==  "0500397" | EMS_ID == "E254309" |
                  EMS_ID == "0500101" | EMS_ID == "E254311" | EMS_ID == "E287172" |
                  EMS_ID == "E318572")

#plot data in specific order
Cu_WC$EMS_ID <- factor(Cu_WC$EMS_ID, 
                       levels=c("E307368", "E266462", "E287173",
                                "E254310", "0500397", "E254309",
                                "0500101", "E254311", "E287172",
                                  "E318572")) 

#Plot cu with chronic guideline
site_Cu2 <- filter(Cu_WC, Variable %in% c("copperdissolved", "Chronic.WQG"))
Cuplot2 <- ggplot(site_Cu2, aes(x = DateTime, y = Value, color = Variable)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Copper"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2)
plot(Cuplot2)
ggsave(filename = "D Cu_Chronic_facet.png", plot = Cuplot2, h=4, w=6, units="in", dpi=300)

#Plot Cu with Acute WQG
site_Cu3 <- filter(Cu_WC, Variable %in% c("copperdissolved", "Acute.WQG"))
Cuplot3 <- ggplot(site_Cu3, aes(x = DateTime, y = Value, color = Variable)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Copper"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2)
plot(Cuplot3)
ggsave(filename = "D Cu_Acute_facet.png", plot = Cuplot3, h=4, w=6, units="in", dpi=300)

