# title: "01b.2_beeReproDev_2021_NestContents_dataset.R"
# author: "Jessie Dodge"
# date: "2024-05-17"
# Purpose: This file is used to average individual nest cell contents measured 
# within Osmia lignaria nests surveyed in 2021 in Boulder County sites
# for projects determining landscape effects on Osmia lignaria parasitism and diet 
# Library #######################################################################################
source(file="01a_libraries.R")

# 2021 Nest data ###########################################################################
nestdata<-read.csv("240220_2021_NestPollenBeeData.csv")

# Clean data ####################################################################################
### change mass to numeric
#nestdata$BeeMass_mg<-as.numeric(nestdata$BeeMass_mg)
nestdata$PollenMass_mg<-as.numeric(nestdata$PollenMass_mg)

### filter out non-O.lig nests
nestdata<-subset(nestdata, Species!="Unk" & Species != "NA")
## Could not find nest T101-A21, and there are no pollen

### filter out incomplete cells
nestdata<-subset(nestdata, Cell_num.New.Old !="0")

# Count Data ####################################################################################
## Total number of Provisioned Cells ###################################
# Determine total number of O. lig provisioned cells per site
# 1) Need to omit empty cells - they should not be included in total count
nestdata<-subset(nestdata, Cell_occupant !="Empty")

# 2) Sum count of occupants within each site
totProvCells<-nestdata %>%  group_by(Site, Habitat) %>%
  tally(., name = "nTotProvised") # this tallies the num of rows within each group
##### Note: 6 sites did not have any O. lig nests

## Successful O. lig Growth per site ###################################
# Determine the number of Successful O. lig Growth Cells per site
### - This means that feeding was initiated and larvae grew to at least 2nd instar
### - Includes parasites, larvae, and pupae. Does NOT include cleptoparasites!
####  -- Successful parasitism (parasite growth) would only have occurred if O.lig offspring had 
          # grown past neonates
# 1) Sum count of all O. lig Larvae, Pupae, and Adult Cell Occupants 
## 1a) might be easier to sum count non-empty O. lig "Life Stage" cells

oligGrowthCells <- nestdata %>% group_by(Site, Habitat) %>%
  subset(Species == "O. lignaria" & Cell_occupant != "Pollen") %>%
  tally(., name = "nOligGrowth") 

## T. stansburyi Cells ###################################
# Determine the number of cells with the cleptoparasites, T. stansburyi
tstanCells<-nestdata %>% group_by(Site, Habitat) %>%
  subset(Species == "T. stansburyi") %>%
  tally(., name = "nTstanCells")

## Parasitized Cells ###################################
# Determine num of parasitized cells, by Monodontomerus spp.
parasiteCells<-nestdata %>% group_by(Site, Habitat) %>%
  subset(Cell_occupant == "Monodontomerus spp.") %>%
  tally(., name = "nParasite")

## Pupae Count ##################################################################################
# Determine number of O. lig  pupae by site
noligP<- nestdata %>% group_by(Site, Habitat) %>%
  subset(Species == "O. lignaria" & Cell_occupant == "Pupae") %>%
  tally(., name = "nOligPupae") 
### F Pupae Count ###################################
# Determine number of O. lig female (F) pupae by site
noligF<- nestdata %>% group_by(Site, Habitat) %>%
  subset(Species == "O. lignaria" & Cell_occupant == "Pupae" & Sex == "F") %>%
  tally(., name = "nOligF") 
### M Pupae Count ###################################
# Determine number of O. lig male (M) pupae by site
noligM<- nestdata %>% group_by(Site, Habitat) %>%
  subset(Species == "O. lignaria" & Cell_occupant == "Pupae" & Sex == "M") %>%
  tally(., name = "nOligM") 
## Larvae Count ##############################################################
noligL<- nestdata %>% group_by(Site, Habitat) %>%
  subset(Species == "O. lignaria" & Cell_occupant == "Larvae") %>%
  tally(., name = "nOligLarve") 

# Mass Data #####################################################################################
## Pupae Mass ###################################
# Determine average O. lig pupae weight by site
oligMass<- nestdata %>% group_by(Site, Habitat) %>%
  subset(Species == "O. lignaria" & Cell_occupant == "Pupae") %>%
  summarise(avgOligMass_mg = mean(BeeMass_mg, na.rm = TRUE))

### F Pupae Mass ######
# Determine average O. lig female (F) pupae weight by site
oligFMass<- nestdata %>% group_by(Site, Habitat) %>%
  subset(Species == "O. lignaria" & Cell_occupant == "Pupae" & Sex == "F") %>%
  summarise(avgOligFMass_mg = mean(BeeMass_mg))
### M Pupae Mass #####
# Determine average O. lig male (M) pupae weight by site
oligMMass<- nestdata %>% group_by(Site, Habitat) %>%
  subset(Species == "O. lignaria" & Cell_occupant == "Pupae" & Sex == "M") %>%
  summarise(avgOligMMass_mg = mean(BeeMass_mg))

# Merged Bee Data ##############################################################################
beedata2<- list(totProvCells, oligGrowthCells, tstanCells, parasiteCells, noligP, noligL,
                noligF, noligM, oligMass, oligFMass, oligMMass)
beedata2 <- beedata2 %>% reduce(full_join, by = c("Site", "Habitat"))
beedata2<-as.data.frame(beedata2)

# Ratio Data ###################################################################################
beedata2$oligRatio <- beedata2$nOligGrowth/beedata2$nTotProvised
beedata2$tstanRatio <- beedata2$nTstanCells/beedata2$nTotProvised
beedata2$monoRatio <- beedata2$nParasite/beedata2$nOligGrowth
beedata2$oligFRatio <- beedata2$nOligF/beedata2$nOligPupae

# Clean data ###############################
beedata2<-as.data.frame(beedata2)
## replace NAs with 0
beedata2[is.na(beedata2)] <- 0
## Add Year
beedata2$Year <- "2021"
## re-org
beedata2<-beedata2[,c(1,2,18,3:17)]

# Clean Environment ############################################################################
### Get rid of extraneous sets
rm(nestdata)
rm(noligP)
rm(noligF)
rm(noligM)
rm(noligL)
rm(oligFMass)
rm(oligGrowthCells)
rm(oligMass)
rm(oligMMass)
rm(parasiteCells)
rm(totProvCells)
rm(tstanCells)
