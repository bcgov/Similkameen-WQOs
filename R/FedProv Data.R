
### Make sure packages are loaded from 01_load.R

##this code is to download all of the fed/prov data
#sites <- wq_sites()

#bc_sites <- sites %>%
# filter(PROV_TERR == "BC") %>%
#pull(SITE_NO)

#all_bc_data <- wq_site_data(bc_sites)
#-----------------------------------------------------------#

##Downloading 2 fed/prov sites for the Similkameen WQOs
##Install canwqdata package to access fed/prov data
#remotes::install_github("bcgov/canwqdata")
library(canwqdata)

##Get data for specific fed/prov sites, name dataframe
wq_site_data(c("BC08NL0001", "BC08NL0005"))

fedprov <- wq_site_data(c("BC08NL0001", "BC08NL0005"))


## Tidies water quality data downloaded from ECCC website
## It retains and renames required columns and sets the timezone to PST.
## Setting <MDL to MDL

fedprov1 <- tidy_ec_data(fedprov, cols = character(0), mdl_action = "mdl")

## Remove variables not of interest to a fresh water analysis, like extractable and air.
fedprov1 <- filter(fedprov1,!grepl('EXTRACTABLE|AIR', Variable))


#Standardize units from ug/L to mg/L for applicable samples
fedprov1 <- fedprov1 %>% mutate(Value=ifelse(Units == "UG/L", Value/1000, Value )) %>%
  mutate(Units = ifelse(Units == "UG/L", "MG/L", Units))


##distinct function to summarize any column in dataframe
parameter<-distinct(fedprov1, Units)


write.csv(fedprov1,
          'C:/R Projects/SimilkameenWQOs/data/all_fedprov.csv', row.names = FALSE)

