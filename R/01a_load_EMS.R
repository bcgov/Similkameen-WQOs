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
library(magrittr)

# Load the last 2 years of water quality data for the Similkameen WQO from the BC Data Catalogue using bcgov/rems package.
# Two year data, four year data, and historic data can be downloaded.
# You can specify  which = "4yr"  to get the last four years of data

twoyear <- get_ems_data(which = "2yr", cols = "all")

Y

filtered_twoyear <- filter_ems_data(twoyear, emsid =
      c("E299452", "0500075", "0500724", "0500725", "E207463", "E317970", "E317971",
      "E318570", "E317973", "E223873", "E223874", "E217193", "E206633", "E206635",
     "E249949", "E249950", "E250424", "E206824", "E206636", "E206637", "E215956",
       "E215957", "E206638", "E215954", "E250751", "E206634",
      "E221413", "E221386", "E221384", "E221387", "E221390", "E221389",
      "E221340", "E221339", "E221341", "0500757", "E221526", "E221525", "0500083","E303710", 
     "E303719", "E318571", "0500073",
     "E266462", "0500101", "E287172", "E318572", "0500003", "1131013", "0500928",
     "E206818"))
     

#remove_data_cache("2yr")

# DOWNLOAD HISTORIC DATA
# If you need to download the historic data, uncomment the following line:
#download_historic_data(ask = FALSE)

# Attach the sq Lite database to R
hist_db <- attach_historic_data()

# Grab data from the Similkameen WQO monitoring sites
filtered_historic <- hist_db %>%
  filter(EMS_ID %in%  c("E299452", "0500075", "0500724", "0500725", "E207463", "E317970", "E317971",
                        "E318570", "E317973", "E223873", "E223874", "E217193", "E206633", "E206635",
                        "E249949", "E249950", "E250424", "E206824", "E206636", "E206637", "E215956",
                        "E215957", "E206638", "E215954", "E250751", "E206634",
                        "E221413", "E221386", "E221384", "E221387", "E221390", "E221389",
                        "E221340", "E221339", "E221341", "0500757", "E221526", "E221525", "0500083","E303710", 
                        "E303714", "E318571", "0500073",
                        "E266462", "0500101", "E287172", "E318572", "0500003", "1131013", "0500928",
                        "E206818"))

# Convert the database object into a regular R data object
filtered_historic <- collect(filtered_historic) %>%
  # Make sure the time data is in proper format
  mutate(COLLECTION_START = ems_posix_numeric(COLLECTION_START),
         COLLECTION_END = ems_posix_numeric(COLLECTION_END))

# Combine the 2yr and the historic dataframes
all_EMS <- bind_ems_data(filtered_twoyear, filtered_historic)

# CREATE CSV OF RAW DATA
write.csv(all_EMS,'C:/R Projects/Similkameen-WQOs/data/report/all_EMS.csv', row.names = FALSE)
