# title: "01i_beeReproDev_MasterData.R"
# author: "Jessie Dodge"
# date: "2024-05-22"
# Purpose: This file is used to combine all data frames into a metadata files for all measurements averaged by site 
# surveyed  Boulder County sites sampled in 2020 and 2021 for projects determining landscape effects 
# on Osmia lignaria parasitism and diet.

# Library #####################################################################################################
source(file="01a_libraries.R")
source(file="01b.3_beeReproDev_NestContents_dataset.R")
source(file="01c_beeReproDev_Flower_dataset.R")
source(file="01d_beeReproDev_Tree_dataset.R")
source(file="01e_beeReproDev_EVT_dataset.R")
source(file="01f_beeReproDev_climate_dataset.R")
source(file="01g_beeReproDev_degreeDay_dataset.R")
source(file="01h_beeReproDev_HLI_dataset.R")
# Merge datasets ############################################################################################
data <- list(sitedata,climdata, beedata, adddata, stemdata, treedata, evtdata.rel)
# Note, if you put beedata first, this omits sites with no bees
data <- data %>% reduce(left_join, by = c("SiteYr","Site", "Habitat", "Year", "Yr"))
data <- as.data.frame(data)

## Clean Data ##################################
# put bee data after elevation
## site vars - 1:5, 10:12, 6:9
## bee release info - 41:43, bee vars - 25:40
## climate vars - 13:24
data <- data[,c(1:5, 41:43, 10:12, 6:9, 25:40, 13:24, 44:76)]

# put JUSC2_L before JUSC2_D
data<-data[,c(1:55, 66,56:65, 67:76)]

# replace NA with 0's for bee data (sites with no bees) and flora data (sites with no flowers)
data[,c(16:26,28:31,48:50)][is.na(data[,c(16:26,28:31,48:50)])] <- 0 

# replace 0 with NA for Tree BA for site T106
data <- data %>% mutate(TotMean_BA = ifelse(Site == "T106" & TotMean_BA == 0, NA, TotMean_BA))

# Row Names #####################################################
rownames(data)<-data[,1]

## Reformat ####################################
data[3:5] <- lapply(data[3:5], as.factor) # make sure vars are numeric
data[9:76] <- lapply(data[9:76], as.numeric) # make sure vars are numeric

# export metadata file to csv
write.table(data, file="tables/beeReproDev_metadata.csv", sep = ",", quote = FALSE, row.names = F)
# Clean Environment ####################################################################################
### Get rid of extraneous sets
rm(adddata)
rm(beedata)
rm(climdata)
rm(evtdata.rel)
rm(sitedata)
rm(stemdata)
rm(treedata)
