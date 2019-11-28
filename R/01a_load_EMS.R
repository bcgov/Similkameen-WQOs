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

# SCRIPT SUMMARY
# Download all EMS data for Similkamen WQO sites
# A file containing the raw data that is loaded and filtered by later scripts


# DOWNLOAD AND LOAD EMS DATA FROM DATA BC OPEN DATA OBJECT

# See rems readme file for more information: https://github.com/bcgov/rems/blob/master/README.Rmd

# install.packages("package name") if not already installed

#install_github("bcgov/rems")
#install_github("bcgov/wqbc", ref = "develop")
devtools::install_github("bcgov/wqbc")

# Load Packages
library(tidyverse)
library(devtools)
library(rems)
library(dplyr)
library(ggplot2)
library(wqbc)
library(lubridate)
library(readr)
library(directlabels)
library(forcats)
library(tidyr)
library(reshape)
library(stats)
library(plyr)


# Load the last 2 years of water quality data for the Similkameen WQO from the BC Data Catalogue using bcgov/rems package.
# Two year data, four year data, and historic data can be downloaded.
# You can specify  which = "4yr"  to get the last four years of data

twoyear <- get_ems_data(which = "2yr", cols = "all")

Y

filtered_twoyear <- filter_ems_data(twoyear, emsid =
      c("E207463", "1131013", "E266462", "E223873", "E223874", "E206818", "E206637",
      "E206636", "E206824", "E250424", "E249950", "E249949", "E206635", "E206634",
     "E250751", "E215954", "E206633", "E217193", "E206638", "E215956", "E215957",
       "E221390", "E221387", "E221386", "E221413", "E221384",
      "E221389", "E221340", "E221339", "E221341", "E221525", "E221526",
      "0500075", "0500724", "0500725", "0500083","E299452", "E299541", "E299450",
     "E303719", "E303720", "E303718", "E303716", "E303715", "E303713", "E303712",
     "E303711", "E303710", "E267982", "E295209", "E239619", "E267984",
     "E303637", "E267987", "E303635", "E267986", "E267985", "E303634",
     "E221502", "E283434", "E303636",
        "0500003", "0500928", "0500757", "0500101", "E307368", "E287171", "E287173",
        "E287174", "E287175", "E287176", "E306784", "E254310", "0500397", "E254309",
      "E254311", "E287172", "0500398", "0500076", "0500416", "E249473", "E307464",
        "E286689", "E287170", "E287169", "E307367", "E206635", "E206824"))

#remove_data_cache("2yr")

# DOWNLOAD HISTORIC DATA
# If you need to download the historic data, uncomment the following line:
#download_historic_data(ask = FALSE)

# Attach the sq Lite database to R
hist_db <- attach_historic_data()

# Grab data from the Similkameen WQO monitoring sites
filtered_historic <- hist_db %>%
  filter(EMS_ID %in% c("E207463", "1131013", "E266462", "E223873", "E223874", "E206818", "E206637",
                       "E206636", "E206824", "E250424", "E249950", "E249949", "E206635", "E206634",
                       "E250751", "E215954", "E206633", "E217193", "E206638", "E215956", "E215957",
                       "E221390", "E221387", "E221386", "E221413", "E221384",
                       "E221389", "E221340", "E221339", "E221341", "E221525", "E221526",
                       "0500075", "0500724", "0500725", "0500083","E299452", "E299541", "E299450",
                       "E303719", "E303720", "E303718", "E303716", "E303715", "E303713", "E303712",
                       "E303711", "E303710", "E267982", "E295209", "E239619", "E267984",
                       "E303637", "E267987", "E303635", "E267986", "E267985", "E303634",
                       "E221502", "E283434", "E303636",
                       "0500003", "0500928", "0500757", "0500101", "E307368", "E287171", "E287173",
                       "E287174", "E287175", "E287176", "E306784", "E254310", "0500397", "E254309",
                       "E254311", "E287172", "0500398", "0500076", "0500416", "E249473", "E307464",
                       "E286689", "E287170", "E287169", "E307367", "E206635", "E206824"))

# Convert the database object into a regular R data object
filtered_historic <- collect(filtered_historic) %>%
  # Make sure the time data is in proper format
  mutate(COLLECTION_START = ems_posix_numeric(COLLECTION_START),
         COLLECTION_END = ems_posix_numeric(COLLECTION_END))

# Combine the 2yr and the historic dataframes
all_EMS <- bind_ems_data(filtered_twoyear, filtered_historic)

# CREATE CSV OF RAW DATA
write.csv(all_EMS,'C:/R Projects/SimilkameenWQOs/data/all_EMS.csv', row.names = FALSE)
