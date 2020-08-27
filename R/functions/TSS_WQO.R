setwd("C:/R Projects/Similkameen-WQOs/R/functions")

source("TSS_WQO.R")

(tsslimits<-TSS_WQO(SR_sites, "0500075", 10, 10, 100))
