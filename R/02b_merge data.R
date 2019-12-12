## load cleaned fedprov (all_data_sim_fedprov) and EMS
## (all_data_sim_EMS) csv files and combine into one
### make sure packages from 01_load.R are loaded if starting here


#Load csv's
fedprov_clean <- read_csv("data/all_fedprov.csv")

EMS_clean <- read_csv("data/similkameen_EMS.csv")


##combine the two dataframes

all_clean <- merge.data.frame(fedprov_clean, EMS_clean, all=TRUE)

## merge columns SITE_NO and EMS_ID so that the fedprov station
## names are in the EMS_ID column

all_similkameen <- transform(all_clean, EMS_ID = ifelse(is.na(EMS_ID), SITE_NO, EMS_ID))

##double check that fedprov stations show up in EMS_ID column
##params <- distinct(fedprov_EMS_sim, EMS_ID)

##remove SITE_NO  and DetectinLimit Column
all_similkameen$SITE_NO <- NULL
all_similkameen$DetectionLimit <- NULL
head(all_similkameen)

# double check that fedprov sites were merged into EMS_ID column
EMS_ID <- distinct (all_similkameen, EMS_ID)


# need to make variable names and units in the same case, choosing lower

all_similkameen <- all_similkameen %>%
  mutate (Variable = tolower(Variable))
all_similkameen <- all_similkameen %>%
  mutate (Units = tolower(Units))


# Look for the parameters we are interested in
## Do this manually by looking at the `parameters` and `all_similkameen` dataframes

parameters <- distinct (all_similkameen, Variable, Units, Value)

## Need to standardize names for SO4- and a few nitrogen parameters

library(plyr)
all_similkameen$Variable <- revalue (all_similkameen$Variable, c("sulphate"="sulfate total", "sulphate dissolved"="sulfate dissolved",
                                                     "nitrogen no2 total"="nitrogen total nitrite",
                                                     "nitrogen no3 total"="nitrogen total nitrate", "nitrogen kjel.tot(n)"="nitrogen total kjeldahl",
                                                     "nitrogen (kjeldahl) total dissolved"="nitrogen dissolved kjeldahl",
                                                     "nitrate (no3) dissolved"= "nitrogen dissolved nitrate",
                                                     "nitrate(no3) + nitrite(no2) dissolved"="nitrogen dissolved no3 & no2",
                                                     "total nitrogen no2 + no3"="nitrogen total no3 & no2", "colour true"="true color",
                                                     "residue: non-filterable (tss)"="tss", "hardness total (total)"="hardness total",
                                                     "ammonia dissolved"="nitrogen ammonia dissolved", "nitrogen nitrite"="nitrogen total nitrite"))


## double check that parameter names were standardized
parameters2 <- distinct (all_similkameen, Variable, Units)

## Standardize a few Units
all_similkameen$Units <- revalue (all_similkameen$Units, c("ug/g"="mg/l", "col.unit"="color unit", "cu"="color unit", "ph units"="ph",
                                               "usie/cm"="us/cm", "c"="deg c"))


write.csv(all_similkameen,
          'C:/R Projects/SimilkameenWQOs/data/reoport/sim_clean.csv', row.names = FALSE)
