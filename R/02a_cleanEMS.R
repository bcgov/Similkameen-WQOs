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

#######################################################################################################################################

### Make sure packages are loaded from 01_load.R

# Load all_EMS csv

all_EMS <- read_csv("data/all_EMS.csv")

# Look at the replicate data to investigate whether it should stay in the data set

## view various ways to name replicates in the SAMPLE_CLASS column
##repdata <- distinct (all_EMS, SAMPLE_CLASS)

# look at how much data there is for samples classified as reglar when replicates taken out
# reg <- all_EMS %>%
#  filter(!SAMPLE_CLASS == "Replicate", !SAMPLE_CLASS == "Replicate-First",
#         !SAMPLE_CLASS == "Replicate-Second", !SAMPLE_CLASS == "Replicate-Third")

# I have decided to proceed without the replicates in the dataset, there are only 199 obs that have been
# classified as replicates and it is unknow if they are QA/QC samples. I considered keeping the bacteriological
# samples but the reps are atleast 15 years old and there were only 18 rep samples compared to 694 regular
# samples...moving on

# result2 <- distinct (all_EMS2, RESULT_LETTER)

## Filter raw data to get rid of replicates and obvious errors prior to cleaning
all_EMS2 <- all_EMS %>%
  # Remove replciate data
  filter(!SAMPLE_CLASS == "Replicate", !SAMPLE_CLASS == "Replicate-First",
               !SAMPLE_CLASS == "Replicate-Second", !SAMPLE_CLASS == "Replicate-Third") %>%
  # Make sure only fresh water samples are retained
  filter(SAMPLE_STATE =="Fresh Water") %>%
  # Remove results that failed q/c
  filter(!QA_INDEX_CODE == "F" | is.na(QA_INDEX_CODE))%>%
  # Remove RESULT_LETTER = M (this stands for missing) and >
  filter(!RESULT_LETTER == "M" | is.na(RESULT_LETTER), !RESULT_LETTER == ">" | is.na(RESULT_LETTER))


## TIDY DATASET
##
## Tidies water quality data downloaded from EMS database using the bcgov/rems package.
## It retains and renames required columns and sets the timezone to PST.
## It sets values that are flagged as being less than the detection limit to the MDL.
## Some values show up in the summary tables as less than the MDL because the RESULT_LETTER column
## did not have "<" in it, so the code did not recognize to convert this value to equal the MDL.
## If RESULT_LETTER has '>', delete those rows as means greater than instrument reading.
## Remove variables not of interest to a fresh water analysis, like Biomass.
tidy_data <- tidy_ems_data(all_EMS2, mdl_action = "mdl")

all_EMS3 <- filter(tidy_data,!grepl('Barometric|Biomass|Flow|Silica|Air|Streptococcus
                                   |Salinity|Tannin|Surfactant|Moisture|Extractable|Extrac.
                                   |Extractble|Extract.|Extractbl', Variable))


#see if above paramters were removed
##params2 <- distinct(all_data_sim_EMS, Variable)

write.csv(all_EMS3,
'C:/R Projects/SimilkameenWQOs/data/similkameen_EMS.csv', row.names = FALSE)
