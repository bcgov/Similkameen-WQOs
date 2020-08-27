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

# This script is to continue assessing censored data, 
# detemine which method to use for censoring, make a table of summary stats

require(tidyverse)
require(magrittr)
require(NADA)
require(lubridate)

setwd("C:/R Projects/Similkameen-WQOs")

# Load SR data
CC_sites <- read.csv("data/report/CC_plots/tables/CC_sites.csv", 
                     stringsAsFactors = FALSE, header=TRUE)

#remove all values <0 (negative values throw an error in ROS)
CC_sites <- CC_sites %>% filter(Value>0)

#remove unnescessary columns
CC_sites <- dplyr::select(CC_sites, -c(Code, ResultLetter, Station)) 

# Summary Stats for table 1 - based upon uncensored data 

table.1 <- CC_sites %>% group_by(EMS_ID, Variable) %>%
  summarise(samples = length(DateTime),
            start.date = min(DateTime),
            end.date = max(DateTime), 
            mean=mean(Value), 
            median=median(Value), 
            sd=sd(Value),
            min=min(Value), 
            max=max(Value),
            quantile95=quantile(Value,probs=0.95, na.rm=TRUE), 
            quantile90=quantile(Value,probs=0.90, na.rm=TRUE),
            quantile25=quantile(Value,probs=0.25, na.rm=TRUE),
            quantile75=quantile(Value,probs=0.75, na.rm=TRUE),
            IQR=IQR(Value, na.rm=TRUE), 
            unit=first(Units), n=n()) %>%
  ungroup()

cens_data <- filter(CC_sites, CENSOR == TRUE)

table.2 <- cens_data %>% group_by(EMS_ID, Variable) %>%
  summarise(N.ND = length(Value),
            ND.Min = min(Value),
            ND.Max = max(Value))

table.1 %<>% left_join(table.2, by = c("EMS_ID", "Variable"))

#write csv for table1
write.csv(table.1,'data/report/CC_plots/tables/Table1_censored.csv', row.names = FALSE)

# Determine non-detects by station to select method.
# Count samples that are D vs ND
method_selection <- CC_sites %>%
  group_by(EMS_ID, Variable) %>%
  summarise(N = length(Value),
            N.ND = length(Value[CENSOR == TRUE])) %>%
  ungroup() %>%
  mutate(PROP.ND = N.ND / N,
         METHOD = ifelse(PROP.ND == 1, "0.5 MDL",
                         ifelse(PROP.ND > 0 & N - N.ND < 3, "0.5 MDL - MEAN",
                                ifelse(PROP.ND > 0, "ROS", "MEAN"))))

# Join to data set
CC_sites %<>% left_join(select(method_selection, EMS_ID, Variable,
                               METHOD))

# Create Table (Breakdown by method)
method_summary <- method_selection %>%
  group_by(METHOD, Variable) %>%
  summarise(STN.COUNT = length(unique(EMS_ID)),
            SMP.COUNT = sum(N))

#write csv for method summary
write.csv(method_summary,'data/report/CC_plots/tables/MethodSummary.csv', row.names = FALSE)


# Calculate Station Means
# GROUP 1 - STATIONS WHERE ALL DATA IS CENSORED, PROPORTION OF ND=1; mean is min MDL=0.00005
# I added the extra quantile steps even though it provides no information for g1_data
# added so that it can be combined with final data summary with g2,g3,g4 data

g1_data <- CC_sites %>% filter(METHOD == "0.5 MDL") %>%
  group_by(EMS_ID, Variable) %>%
  summarise(r_mean = signif(min(Value / 2), 3), #takes 1/2 of minimum MDL
            r_med = signif(median(Value, na.rm = TRUE), 3),
            r_95 = signif(quantile(Value, prob=0.95, na.rm = TRUE), 3),
            r_90 = signif(quantile(Value, prob=0.90, na.rm = TRUE), 3),
            r_10 = signif(quantile(Value, prob=0.10, na.rm = TRUE), 3),
            r_25 = signif(quantile(Value, prob=0.25, na.rm = TRUE), 3),
            r_75 = signif(quantile(Value, prob=0.75, na.rm = TRUE), 3),
            r_min = signif(min(Value, na.rm = TRUE), 3), #indicates this value is half lowest MDL
            r_max = signif(max(Value, na.rm = TRUE), 3),
            r_n = length(Value)) %>%
  ungroup() %>%
  mutate(CENSOR.METHOD = "0.5 MDL")

# GROUP 2 - Stations where ND < 3, substitute detection limit,  and calculate
# arithmetic mean
g2_data <- CC_sites %>% filter(METHOD == "0.5 MDL - MEAN") %>%
  mutate(CENS.VAL = ifelse(CENSOR == TRUE, signif(Value / 2, 3),
                           Value)) %>%
  group_by(EMS_ID, Variable) %>%
  summarise(r_mean = signif(mean(CENS.VAL, 3)),
            r_med = signif(median(CENS.VAL, na.rm = TRUE), 3),
            r_95 = signif(quantile(CENS.VAL, prob=0.95, na.rm = TRUE), 3),
            r_90 = signif(quantile(CENS.VAL, prob=0.90, na.rm = TRUE), 3),
            r_10 = signif(quantile(CENS.VAL, prob=0.10, na.rm = TRUE), 3),
            r_25 = signif(quantile(CENS.VAL, prob=0.25, na.rm = TRUE), 3),
            r_75 = signif(quantile(CENS.VAL, prob=0.75, na.rm = TRUE), 3),
            r_min = signif(min(CENS.VAL, na.rm = TRUE), 3), #indicates this value is half lowest MDL
            r_max = signif(max(CENS.VAL, na.rm = TRUE), 3),
            r_n = length(CENS.VAL))%>%
  ungroup() %>%
  mutate(CENSOR.METHOD = "0.5 MDL - MEAN")

# GROUP 3 - STATIONS WHERE %ND is >0% and <100% AND D >= 3, calculate station
# means using ros in package NADA
g3_data <- CC_sites %>% filter(METHOD == "ROS")

# Create function for running ROS and returning the calculated mean in a data
# frame (do() requires that a data frame is returned)

calc_ros_mean <- function(result, censor) {
  
  x_ros <- ros(result, censor)
  r_mean <- signif(mean(x_ros, na.rm = TRUE), 3)
  r_med <- signif(median(x_ros, na.rm = TRUE), 3)
  r_95 <- signif(quantile(x_ros, prob=0.95, na.rm = TRUE), 3)
  r_90 <- signif(quantile(x_ros, prob=0.90, na.rm = TRUE), 3)
  r_10 <- signif(quantile(x_ros, prob=0.10, na.rm = TRUE), 3)
  r_25 <- signif(quantile(x_ros, prob=0.25, na.rm = TRUE), 3)
  r_75 <- signif(quantile(x_ros, prob=0.75, na.rm = TRUE), 3)
  r_min <- signif(min(x_ros$modeled, na.rm = TRUE), 3)
  r_max <- signif(max(x_ros$modeled, na.rm = TRUE), 3)
  
  return(data.frame(r_mean, r_med, r_95, r_90, r_10, r_25, r_75, r_min, r_max))
  
}

#######################################################
# Run analysis

g3_data %<>%
  group_by(EMS_ID, Variable) %>%
  do(calc_ros_mean(.$Value, .$CENSOR)) %>%
  ungroup() %>%
  mutate(CENSOR.METHOD = "ROS")

#the n() was not working within the calc_ros_mean function, had to add it afterwards 
g3_data %<>% left_join(select(method_selection, EMS_ID, Variable,
                              N))
names(g3_data)[names(g3_data)=="N"]<- "r_n" #renaming column to match g1,g2,g4_data

# GROUP 4 - Stations where ND = 0%
g4_data <- CC_sites %>% filter(METHOD == "MEAN") %>%
  group_by(EMS_ID, Variable) %>%
  summarise(r_mean = signif(mean(Value, 3)),
            r_med = signif(median(Value, na.rm = TRUE), 3),
            r_95 = signif(quantile(Value, prob=0.95, na.rm = TRUE), 3),
            r_90 = signif(quantile(Value, prob=0.90, na.rm = TRUE), 3),
            r_10 = signif(quantile(Value, prob=0.10, na.rm = TRUE), 3),
            r_25 = signif(quantile(Value, prob=0.25, na.rm = TRUE), 3),
            r_75 = signif(quantile(Value, prob=0.75, na.rm = TRUE), 3),
            r_min = signif(min(Value, na.rm = TRUE), 3), #indicates this value is half lowest MDL
            r_max = signif(max(Value, na.rm = TRUE), 3),
            r_n = length(Value)) %>%
  ungroup() %>%
  mutate(CENSOR.METHOD = "MEAN")

# COMBINE MEANS INTO TABLE & CALCULATE SUMMARY STATS
summary_stats <- bind_rows(g1_data, g2_data, g3_data, g4_data)
rm(g1_data, g2_data, g3_data, g4_data)

#Modify table if you want to add # of ND and units back in 
summary_stats %<>% left_join(select(table.1, EMS_ID, Variable, 
                                    N.ND, unit)) %>%
  mutate(PROP.N = signif(r_n / N.ND), 3)


#write csv for summary stats
write.csv(summary_stats,'data/report/CC_plots/tables/SummaryStats.csv', row.names = FALSE)

####################################################################################################################

# make box plots from the summary stats

rm(list=ls(all=TRUE))

#set working drive to get summary stats spreadsheet
setwd("C:/R Projects/Similkameen-WQOs")

#load summary stats table
sumstats <- read.csv("data/report/CC_plots/tables/SummaryStats.csv", 
                     stringsAsFactors = FALSE, header=TRUE)

## order sites from upstream to downstream
sumstats$EMS_ID <- factor(sumstats$EMS_ID, 
                          levels=c( "E206635", "E249949", "E249950", "E250424", 
                                    "E206824", "E206636", "E206637"))

## First set working directory to save plots to. 
setwd('C:/R Projects/Similkameen-WQOs/data/report/CC_Plots/WQOs')

parameters <- as.character(unique(sumstats$Variable))

#run box plot loop
for (i in 1:length(parameters)){
  
  x <- parameters[i]
  p <- sumstats %>% filter(Variable == x)
  
  ggplot(p, aes(as.factor(EMS_ID))) +
    geom_boxplot(aes(
      lower = r_10, 
      upper = r_90, 
      middle = r_mean, 
      ymin = r_min - r_min*0.5, 
      ymax = r_max + r_max*0.5),
      stat = "identity") +
    #geom_hline(WQG-AL-ST, color="blue", size=1, linetype=2)+
    #geom_hline(WQG-AL-LT, color="darkblue", size=1, linetype=2)+
    #geom_hline(WQO-Old, color="black", size=1, linetype=2)+
    #geom_hline(WQG-2015, color="darkgrey", size=1, linetype="dotted")+
    xlab("EMS_ID") +
    ylab(paste0(x)) +
    theme_bw()
  
  ggsave(filename=paste0("C:/R Projects/Similkameen-WQOs/data/report/CC_Plots/",x,".tiff"), units="in", width=9, height=6, dpi=300, compression = 'lzw')
  
  print(paste0("Done figure ", x))
}


praise::praise()
