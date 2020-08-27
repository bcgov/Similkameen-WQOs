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
plotdata <- read_csv("data/report/KR_Plots/tables/KR_sites2.csv")

NH4WQGs <- read_csv(file="data/report/NH4_guidelines_long.csv",
                    col_types = cols_only(EMS_ID=col_character(),
                                          DateTime=col_character(),
                                          ResultLetter=col_character(),
                                          Variable=col_factor(),
                                          Value=col_double(),
                                          CENSOR=col_factor() )) %>%
  mutate(
    DateTime=as.POSIXct(DateTime, format="%Y-%m-%d", tz="UTC"))

#no copper BLM results for KR stations


## change case of censor column
plotdata$CENSOR <- gsub("FALSE", "False", plotdata$CENSOR)
plotdata$CENSOR <- gsub("TRUE", "True", plotdata$CENSOR)


## order sites from upstream to downstream
plotdata$EMS_ID <- factor(plotdata$EMS_ID, 
                          levels=c( "E221413", "E221386", "E221384", "E221387", 
                                    "E221390", "E221389", "E221339", "E221340",
                                    "E221341", "0500757"))

#units <- distinct(plotdata, Units)

# set working drive for plots
setwd('C:/R Projects/Similkameen-WQOs/data/report/KR_Plots/WQOs')

## PLOTTING

#dissolved chloride
site_cl <- filter(plotdata, Variable %in% "chloride dissolved")
clplot <- ggplot(site_cl, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point() + 
  geom_hline(aes(yintercept = 100,linetype = "Acute"), color = "red") +
  scale_linetype_manual(name = "WQO", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  xlab("Date") +
  ylab("Dissolved Chloride (mg/L)") + 
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")
plot(clplot)
ggsave(filename = "DCl.png", plot = clplot, h=4, w=6, units="in", dpi=300)

#facet for chloride dissolved

site_cl <- filter(plotdata, Variable %in% "chloride dissolved")
clplot <- ggplot(site_cl, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Chloride (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(clplot)
ggsave(filename = "DCl_facet.png", plot = clplot, h=4, w=6, units="in", dpi=300)

#nitrite and nitrate
site_nn <- filter(plotdata, Variable %in% "nitrite and nitrate dissolved")
nnplot <- ggplot(site_nn, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point() + 
  geom_hline(aes(yintercept = 10,linetype = "Acute"), color = "red") +
  scale_linetype_manual(name = "WQO", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  xlab("Date") +
  ylab("Dissolved Nitrite and Nitrate (mg/L)") + 
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")
plot(nnplot)
ggsave(filename = "nitrite_nitrate.png", plot = nnplot, h=4, w=6, units="in", dpi=300)

#facet for nitrite and nitrate

site_nn <- filter(plotdata, Variable %in% "nitrite and nitrate dissolved")
nnplot <- ggplot(site_nn, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Dissolved Nitrite and Nitrate (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(nnplot)
ggsave(filename = "nitrate_nitrite_facet.png", plot = nnplot, h=4, w=6, units="in", dpi=300)

#e.coli
site_ecoli <- filter(plotdata, Variable %in% "e coli")
ecoliplot <- ggplot(site_ecoli, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  xlab("Date") +
  ylab("E. Coli. (cfu/100mL)") +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape = "<MDL")
plot(ecoliplot)
ggsave(filename = "e.coli.png", plot = ecoliplot, h=4, w=6, units="in", dpi=300)

site_ecoli <- filter(plotdata, Variable %in% "e coli")
ecoliplot <- ggplot(site_ecoli, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("E. Coli. (cfu/100mL)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(ecoliplot)
ggsave(filename = "ecoli_facet.png", plot = ecoliplot, h=4, w=6, units="in", dpi=300)

#fecal coliform
site_fc <- filter(plotdata, Variable %in% "fecal coliform")
fcplot <- ggplot(site_fc, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  xlab("Date") +
  ylab("Fecal Coliform (cfu/100mL)") + 
  theme(text=element_text(size=8)) +
  labs(color="Station", shape = "<MDL")
plot(fcplot)
ggsave(filename = "fecal coliform.png", plot = fcplot, h=4, w=6, units="in", dpi=300)

site_fc <- filter(plotdata, Variable %in% "fecal coliform")
fcplot <- ggplot(site_fc, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Fecal Coliform (cfu/100mL)") + coord_cartesian(ylim = c(0,500)) +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(fcplot)
ggsave(filename = "fecal coliform_facet.png", plot = fcplot, h=4, w=6, units="in", dpi=300)

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

site_tss <- filter(plotdata, Variable %in% "tss")
tssplot <- ggplot(site_tss, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key = element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  xlab("Date") +
  ylab("TSS (mg/L)") + coord_cartesian(ylim = c(0,300)) +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")
plot(tssplot)
ggsave(filename = "tss2.png", plot = tssplot, h=4, w=6, units="in", dpi=300)

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

site_tss <- filter(plotdata, Variable %in% "tss")
tssplot <- ggplot(site_tss, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("TSS (mg/L)") + coord_cartesian(ylim = c(0, 300)) +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(tssplot)
ggsave(filename = "tss_facet2.png", plot = tssplot, h=4, w=6, units="in", dpi=300)

#look at TSS data 
tss_data<- filter(plotdata, Variable %in% "tss")
tss_data %>% group_by(EMS_ID) %>%
  summarise(min=min(Value), max=max(Value), mean=mean(Value)) %>% View(.)

#load TSS function and then run

(tsslimits<-TSS_WQO(plotdata, "E221413", 10, 10, 100))

#write csv of TSS threshold, data shows clear flows for threshold is 10 mg/L 
write.csv(tsslimits,'C:/R Projects/Similkameen-WQOs/data/report/KR_Plots/tables/SR_tss.csv', row.names = FALSE)

## plot tss against threshold for E221413

tssplot <- ggplot(site_tss, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) +
  geom_line(data=tsslimits %>% select(-EMS_ID), aes(x = DateTime, y=Threshold), color="darkred") +
  theme_bw() +
  xlab("Date") +
  ylab("TSS (mg/L)") + coord_cartesian(ylim = c(0, 300)) +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(tssplot)
ggsave(filename = "tss_facetWQO.png", plot = tssplot, h=4, w=6, units="in", dpi=300)

#Total Hardness 
site_hard <- filter(plotdata, Variable %in% "hardness total")
hardplot <- ggplot(site_hard, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point(size=0.75) + 
  geom_point() + 
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key= element_rect(fill="NA")) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("Total Hardness (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(hardplot)
ggsave(filename = "hardnessT.png", plot = hardplot, h=4, w=6, units="in", dpi=300)

# Facet for Dissolved Hardness

site_hard <- filter(plotdata, Variable %in% "hardness total")
hardplot <- ggplot(site_hard, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Hardness (mg/L)") +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(hardplot)
ggsave(filename = "hardnessT_facet.png", plot = hardplot, h=4, w=6, units="in", dpi=300)

#Total Phosphorus


site_P <- filter(plotdata, Variable %in% "phosphorus total")
Pplot <- ggplot(site_P, aes(x = DateTime, y = Value, color = EMS_ID, shape=CENSOR)) +
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
Pplot <- ggplot(site_P, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Phosphorus (mg/L)") +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(Pplot)
ggsave(filename = "TP_facet.png", plot = Pplot, h=4, w=6, units="in", dpi=300)

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

# Total Dissolved Phosphorus Facet

site_P <- filter(plotdata, Variable %in% "phosphorus total dissolved")
Pplot <- ggplot(site_P, aes(x = DateTime, y = Value, shape=CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Dissolved Phosphorus (mg/L)") +
  labs(shape="<MDL") +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(Pplot)
ggsave(filename = "DP_facet.png", plot = Pplot, h=4, w=6, units="in", dpi=300)

# Total Arsenic facet

site_as <- filter(plotdata, Variable %in% "arsenic total")
asplot <- ggplot(site_as, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 5,linetype = "Acute AL"), color = "red") +
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

# Total Fe facet

site_fe <- filter(plotdata, Variable %in% "iron total")
feplot <- ggplot(site_fe, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 1,linetype = "Acute AL"), color = "blue") +
  geom_hline(aes(yintercept = 0.3,linetype = "DW"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("blue", "red")))) +
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

# Total Molybdenum

site_mo <- filter(plotdata, Variable %in% "molybdenum total") 
moplot <- ggplot(site_mo, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.03,linetype = "Chronic IR"), color = "red") +
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

# Total U facet

site_ur <- filter(plotdata, Variable %in% "uranium total") 
urplot <- ggplot(site_ur, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) +
  geom_hline(aes(yintercept = 8.5,linetype = "Chronic AL"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
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
ggsave(filename = "TUr_facet2.png", plot = urplot, h=4, w=6, units="in", dpi=300)

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

site_se <- filter(plotdata, Variable %in% "selenium total")
seplot <- ggplot(site_se, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
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
ggsave(filename = "TSe_facet2.png", plot = seplot, h=4, w=6, units="in", dpi=300)

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
  geom_hline(aes(yintercept = 11,linetype = "Salmonid Embryos Present"), color = "red") +
  geom_hline(aes(yintercept = 8,linetype = "Min"), color = "red") +
  scale_linetype_manual(name = "WQO", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("red", "red")))) +
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("Dissolved Oxygen (mg/L)") + 
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(doplot)
ggsave(filename = "DO.png", plot = doplot, h=4, w=6, units="in", dpi=300)

# Dissolved oxygen - field

site_do<- filter(plotdata, Variable %in% "dissolved oxygen-field")
doplot <- ggplot(site_do, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 11,linetype = "Salmonid Embryos Present"), color = "red") +
  geom_hline(aes(yintercept = 8,linetype = "Min"), color = "red") +
  scale_linetype_manual(name = "WQO", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("red", "red")))) +
  theme_classic() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  xlab("Date") +
  ylab("Dissolved Oxygen (mg/L)") + 
  theme(text=element_text(size=8)) +
  labs(color="Station")
plot(doplot)
ggsave(filename = "DO Field.png", plot = doplot, h=4, w=6, units="in", dpi=300)

# Total Al facet

site_al <- filter(plotdata, Variable %in% "aluminum total")
alplot <- ggplot(site_al, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 9.5,linetype = "Max DW"), color = "blue") +
  geom_hline(aes(yintercept = 5,linetype = "Acute WL/LS/IR"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(2, 2),  
                        guide = guide_legend(override.aes = list(color = c("red", "blue")))) +
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

# total Co facet

site_co <- filter(plotdata, Variable %in% "cobalt total")
Coplot <- ggplot(site_co, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") +
  ylab("Total Cobalt"~~(mu*g/L)) + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(Coplot)
ggsave(filename = "TCo_facet.png", plot = Coplot, h=4, w=6, units="in", dpi=300)

# Total Silver
#guideline based on hardness <100 which is what the reference station is

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
  theme(text=element_text(size=8)) + ylim(0,0.1) +
  labs(shape="<MDL")+
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(agplot)
ggsave(filename = "TAg_facet.png", plot = agplot, h=4, w=6, units="in", dpi=300)

# Total Mn facet

site_mn <- filter(plotdata, Variable %in% "manganese total")
mnplot <- ggplot(site_mn, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 0.8,linetype = "hardness 50 mg/L"), color = "red") +
  scale_linetype_manual(name = "WQG Chronic", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_bw() +
  xlab("Date") +
  ylab("Total Manganese (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(mnplot)
ggsave(filename = "TMn_facet.png", plot = mnplot, h=4, w=6, units="in", dpi=300)

# Total Pb facet

site_pb <- filter(plotdata, Variable %in% "lead total")
pbplot <- ggplot(site_pb, aes(x = DateTime, y = Value)) +
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

# Total zn facet

site_zn <- filter(plotdata, Variable %in% "zinc total")
znplot <- ggplot(site_zn, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 7.5,linetype = "Chronic A:"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
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

# Dissolved Sulphate

# Dissolved SO42- facet

site_so4 <- filter(plotdata, Variable %in% "sulfate dissolved")
so4plot <- ggplot(site_so4, aes(x = DateTime, y = Value, shape = CENSOR)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 218,linetype = "AL (hardness 31-75)"), color = "red") +
  scale_linetype_manual(name = "WQG Chronic", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = c("red")))) +
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

# Temperature
site_t <- filter(plotdata, Variable %in% "temperature-field")
tplot <- ggplot(site_t, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_hline(aes(yintercept = 17,linetype = "weekly ave"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
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

#Field temp facet
site_t <- filter(plotdata, Variable %in% "temperature-field")
tplot <- ggplot(site_t, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) + 
  geom_hline(aes(yintercept = 17,linetype = "weekly ave"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_bw() +
  xlab("Date") +
  ylab("Field Temperature (°C)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(tplot)
ggsave(filename = "field temp facet.png", plot = tplot, h=4, w=6, units="in", dpi=300)

#Field temp facet
site_t <- filter(plotdata, Variable %in% "temperature")
tplot <- ggplot(site_t, aes(x = DateTime, y = Value)) +
  geom_point(size=0.75) +
  geom_hline(aes(yintercept = 17,linetype = "weekly ave"), color = "red") +
  scale_linetype_manual(name = "WQG", values = c(1, 1),  
                        guide = guide_legend(override.aes = list(color = "red"))) +
  theme_bw() +
  xlab("Date") +
  ylab("Temperature (°C)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(tplot)
ggsave(filename = "temp facet.png", plot = tplot, h=4, w=6, units="in", dpi=300)

# Dissolved Organic Carbon
site_doc <- filter(plotdata, Variable %in% "carbon dissolved organic")
docplot <- ggplot(site_doc, aes(x = DateTime, y = Value, color = EMS_ID)) +
  geom_point(size=0.75) + 
  theme_bw() +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  xlab("Date") +
  ylab("2010 Dissolved Organic Carbon (mg/L)") +
  theme(text=element_text(size=8)) +
  labs(color="Station", shape="<MDL")+
  facet_wrap(~Variable, ncol=2)
plot(docplot)
ggsave(filename = "DOC.png", plot = docplot, h=4, w=6, units="in", dpi=300)

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
  #removing 4 values pre-2010 >100
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
  ylab("Turbidity (NTU)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(strip.text = element_text(face="bold", size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  theme(text=element_text(size=8)) +
  labs(shape="<MDL") +
  facet_wrap(~EMS_ID, ncol=2, drop=FALSE)
plot(ntuplot)
ggsave(filename = "turbidity_facet.png", plot = ntuplot, h=4, w=6, units="in", dpi=300)

# load Turbidity function and then run

ntu_WQO <- turbidity_WQO(plotdata, "E221413", 2, 5, 10, 8, 50)

#write csv of turbidity threshold, data shows clear flows for threshold is 10 mg/L 
write.csv(ntu_WQO,'C:/R Projects/Similkameen-WQOs/data/report/KR_Plots/tables/SR_turbidity.csv', row.names = FALSE)

## plot turbidity against threshold for E221413

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
ggsave(filename = "tubidityWQO_facet.png", plot = ntuplot2, h=4, w=6, units="in", dpi=300)

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

# Facet of NH4 with guidelines 

## make dataframe of Keremeos River sites
KR_sites <- filter(NH4WQGs, EMS_ID == "E221386" | EMS_ID == "E221384" | EMS_ID == "E221413" |
                     EMS_ID == "E221387" | EMS_ID ==  "E221390" | EMS_ID == "E221389" |
                     EMS_ID == "E221339" | EMS_ID == "E221340" | EMS_ID == "E221341" |
                     EMS_ID == "0500757")

## order sites from upstream to downstream
KR_sites$EMS_ID <- factor(KR_sites$EMS_ID, 
                          levels=c( "E221413", "E221386", "E221384", "E221387", 
                                    "E221390", "E221389", "E221339", "E221340",
                                    "E221341", "0500757"))


NH4plot2 <- ggplot(KR_sites, aes(x = DateTime, y = Value, color = Variable, shape = CENSOR)) +
  geom_point(size=0.75) + 
  theme_bw() +
  xlab("Date") + 
  ylab("Dissolved Ammonia (mg/L)") + 
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.2, "cm")) + theme(legend.key = element_blank()) +
  theme(text=element_text(size=8)) +
  theme(strip.text = element_text(size=4, margin = margin(.2, 0, .2, 0, "mm"))) +
  labs(shape="<MDL")+
  facet_wrap(~EMS_ID, ncol=2)
plot(NH4plot2)
ggsave(filename = "D NH4_WQG_facet.png", plot = NH4plot2, h=4, w=6, units="in", dpi=300)


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

