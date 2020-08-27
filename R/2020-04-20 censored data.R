# Combine EMS and CABIN data, identify outliers, and summarize data by region

require(rems)
require(wqbc)
require(ggplot2)
require(tidyr)
require(dplyr)
require(magrittr)
require(NADA)
require(lubridate)



rm(list=ls(all=TRUE))

setwd("H:\\ColumbiaWQO_DB\\")


db <- read.csv(file="ColumbiaWQODB_Mar31.csv", header=TRUE, 
               fileEncoding="UTF-8-BOM")

db$date <- as_date(db$date)

working_data <- db 



#this is a fix for my db 
working_data <- dplyr::filter(working_data,
                         !is.na(Parameter))


## MODIFY BASED UPON NEW WORKING DATASET
# Summary Stats for table 1 - based upon uncensored data 

table.1 <- working_data %>% group_by(Section, Parameter) %>%
  summarise(stations = length(unique(Location.ID)),
            samples = length(date),
            start.date = min(date),
            end.date = max(date), 
            mean=mean(Mod.Measurement), 
            median=median(Mod.Measurement), 
            sd=sd(Mod.Measurement),
            min=min(Mod.Measurement), 
            max=max(Mod.Measurement),
            quantile95=quantile(Mod.Measurement,probs=0.95, na.rm=TRUE), 
            quantile90=quantile(Mod.Measurement,probs=0.90, na.rm=TRUE),
            quantile25=quantile(Mod.Measurement,probs=0.25, na.rm=TRUE),
            quantile75=quantile(Mod.Measurement,probs=0.75, na.rm=TRUE),
            IQR=IQR(Mod.Measurement, na.rm=TRUE), 
            unit=first(Guideline.Unit), n=n()) %>%
    ungroup()

#################################################################

cens_data <- filter(working_data, Below.RDL == TRUE)

table.2 <- cens_data %>% group_by(Section, Parameter) %>%
  summarise(N.ND = length(Mod.Measurement),
            ND.MIN = min(Mod.Measurement),
            ND.MAX = max(Mod.Measurement))

table.1 %<>% left_join(table.2, by = c("Section", "Parameter"))

write.csv(table.1, file="table1.csv")

# Determine non-detects by station to select method.
# Count samples that are D vs ND
method_selection <- working_data %>%
  group_by(Section, Parameter) %>%
  summarise(N = length(Mod.Measurement),
            N.ND = length(Mod.Measurement[Below.RDL == TRUE])) %>%
  ungroup() %>%
  mutate(PROP.ND = N.ND / N,
         METHOD = ifelse(PROP.ND == 1, "0.5 MDL",
                         ifelse(PROP.ND > 0 & N - N.ND < 3, "0.5 MDL - MEAN",
                                ifelse(PROP.ND > 0 & N - N.ND > 3, "ROS", "MEAN"))))


# Join to data set
working_data %<>% left_join(select(method_selection, Section, Parameter,
                                    METHOD))

write.csv(method_selection, file="method_selection.csv")


# Create Table 6.1 (Breakdown by method)
method_summary <- method_selection %>%
  group_by(METHOD, Parameter) %>%
  summarise(STN.COUNT = length(unique(Section)),
            SMP.COUNT = sum(N))

write.csv(method_summary, file="method_summary.csv")



# Calculate Station Means
# GROUP 1 - STATIONS WHERE ALL DATA IS CENSORED, PROPORTION OF ND=1; mean is min MDL=0.00005
# I added the extra quantile steps even though it provides no information for g1_data
# added so that it can be combined with final data summary with g2,g3,g4 data

g1_data <- working_data %>% filter(METHOD == "0.5 MDL") %>%
  group_by(Section, Parameter) %>%
  summarise(r_mean = signif(min(Mod.Measurement / 2), 3), #takes 1/2 of minimum MDL
            r_med = signif(median(Mod.Measurement, na.rm = TRUE), 3),
            r_95 = signif(quantile(Mod.Measurement, prob=0.95, na.rm = TRUE), 3),
            r_90 = signif(quantile(Mod.Measurement, prob=0.90, na.rm = TRUE), 3),
            r_10 = signif(quantile(Mod.Measurement, prob=0.10, na.rm = TRUE), 3),
            r_25 = signif(quantile(Mod.Measurement, prob=0.25, na.rm = TRUE), 3),
            r_75 = signif(quantile(Mod.Measurement, prob=0.75, na.rm = TRUE), 3),
            r_min = signif(min(Mod.Measurement, na.rm = TRUE), 3), #indicates this value is half lowest MDL
            r_max = signif(max(Mod.Measurement, na.rm = TRUE), 3),
            r_n = length(Mod.Measurement)) %>%
  ungroup() %>%
  mutate(CENSOR.METHOD = "0.5 MDL")

# GROUP 2 - Stations where ND < 3, substitute detection limit,  and calculate
# arithmetic mean
g2_data <- working_data %>% filter(METHOD == "0.5 MDL - MEAN") %>%
  mutate(CENS.VAL = ifelse(Below.RDL == TRUE, signif(Mod.Measurement / 2, 3),
                           Mod.Measurement)) %>%
  group_by(Section, Parameter) %>%
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
g3_data <- working_data %>% filter(METHOD == "ROS")

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
  group_by(Section, Parameter) %>%
  do(calc_ros_mean(.$Mod.Measurement, .$Below.RDL)) %>%
  ungroup() %>%
  mutate(CENSOR.METHOD = "ROS")

#the n() was not working within the calc_ros_mean function, had to add it afterwards 
g3_data %<>% left_join(select(method_selection, Section, Parameter,
                                   N))
names(g3_data)[names(g3_data)=="N"]<- "r_n" #renaming column to match g1,g2,g4_data

# GROUP 4 - Stations where ND = 0%
g4_data <- working_data %>% filter(METHOD == "MEAN") %>%
  group_by(Section, Parameter) %>%
  summarise(r_mean = signif(mean(Mod.Measurement, 3)),
            r_med = signif(median(Mod.Measurement, na.rm = TRUE), 3),
            r_95 = signif(quantile(Mod.Measurement, prob=0.95, na.rm = TRUE), 3),
            r_90 = signif(quantile(Mod.Measurement, prob=0.90, na.rm = TRUE), 3),
            r_10 = signif(quantile(Mod.Measurement, prob=0.10, na.rm = TRUE), 3),
            r_25 = signif(quantile(Mod.Measurement, prob=0.25, na.rm = TRUE), 3),
            r_75 = signif(quantile(Mod.Measurement, prob=0.75, na.rm = TRUE), 3),
            r_min = signif(min(Mod.Measurement, na.rm = TRUE), 3), #indicates this value is half lowest MDL
            r_max = signif(max(Mod.Measurement, na.rm = TRUE), 3),
            r_n = length(Mod.Measurement)) %>%
  ungroup() %>%
  mutate(CENSOR.METHOD = "MEAN")

# COMBINE MEANS INTO TABLE,ADD GEOGRAPHIC INFO & CALCULATE SUMMARY STATS
summary_stats <- bind_rows(g1_data, g2_data, g3_data, g4_data)
rm(g1_data, g2_data, g3_data, g4_data)

#Modify table if you want to add # of ND and units back in 
summary_stats %<>% left_join(select(table.1, Section, Parameter, 
                             N.ND, unit)) %>%
                    mutate(PROP.N = signif(r_n / N.ND), 3)


write.csv(summary_stats, file="Censored_SummaryStats.csv")




                          