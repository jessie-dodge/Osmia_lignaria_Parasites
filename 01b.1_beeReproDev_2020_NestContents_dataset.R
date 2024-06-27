# title: "01b.1_beeReproDev_2020_NestContents_dataset.R"
# author: "Jessie Dodge"
# date: "2024-05-17"
# Purpose: This file is used to average individual nest cell contents measured 
# within Osmia lignaria nests surveyed in 2020 in Boulder County sites 
# for projects determining landscape effects on Osmia lignaria parasitism and diet 
# Library ####################################################################################
source(file="01a_libraries.R")

# 2020 Nest data ###########################################################################
nestdata<-read.csv("240219_2020_NestPollenBeeData.csv")

# Clean data #################################################################################
### change mass to numeric
nestdata$BeeMass_mg<-as.numeric(nestdata$BeeMass_mg)
nestdata$PollenMass_mg<-as.numeric(nestdata$PollenMass_mg)

### make unique ID for Nests collected in early July (E) vs late July (L)
nestdata$Collected<-as.factor(ifelse(nestdata$DateCollected == "2020-07-26" | 
                                       nestdata$DateCollected == "2020-07-27",
                                     "L", "E"))
nestdata$DateBoxNestID<-paste(nestdata$Collected, nestdata$BoxNest_ID,sep = "-")

#str(nestdata) # to check

### filter out non-O.lig nests
nestdata<-subset(nestdata, Species!="Unk" & Species != "NA")

### filter out incomplete cells
nestdata<-subset(nestdata, Cell_num.New.Old !="0")

# Count Data #################################################################################
## Total number of Provisioned Cells ###################################
# Determine total number of O. lig provisioned cells per site
# 1) Need to omit empty cells - they should not be included in total count
nestdata<-subset(nestdata, Cell_occupant !="Empty")
# 2) Sum count of occupants within each site

#totProvCells<-nestdata %>%  group_by(DateBoxNestID) %>%
#    tally() 
##### Note: Nest H18-A19 had O. lig occupants collected both in Early and Late July, making 
            # it the only duplicated nest
##### Should we just combine? It was probably the same female continuing the nest I collected
              # prematurely

totProvCells<-nestdata %>%  group_by(Site, Habitat) %>%
  tally(., name = "nTotProvised") # this tallies the num of rows within each group
##### Note: H03 did not have any O. lig nests, hence why there are only 14 obs (15 sites) 

## Successful O. lig Growth per site ###################################
# Determine the number of Successful O. lig Growth Cells per site
### - This means that feeding was initiated and larvae grew to at least 2nd instar
### - Includes parasites, larvae, and pupae. Does NOT include cleptoparasites!
####  -- Successful parasitism (parasite growth) would only have occurred if O.lig offspring 
          # had grown past neonates
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

## Pupae Count ################################################################
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

# Mass Data #################################################################################
## Pupae Mass ###################################
# Determine average O. lig pupae weight by site
oligMass<- nestdata %>% group_by(Site, Habitat) %>%
  subset(Species == "O. lignaria" & Cell_occupant == "Pupae") %>%
  summarise(avgOligMass_mg = mean(BeeMass_mg))
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

## Larvae Mass ###################################
# Determine average O. lig larvae weight by site
oligLarvMass<- nestdata %>% group_by(Site, Habitat) %>%
  subset(Species == "O. lignaria" & Cell_occupant == "Larvae") %>%
  summarise(avgOligLarvMass_mg = mean(BeeMass_mg))

# Merged Bee Data ###########################################################################
beedata1<- list(totProvCells, oligGrowthCells, tstanCells, parasiteCells, noligP, noligL,
                  noligF, noligM, oligMass, oligFMass, oligMMass, oligLarvMass)
beedata1 <- beedata1 %>% reduce(full_join, by = c("Site", "Habitat"))
beedata1 <- as.data.frame(beedata1)

## Rename site C06 tp T106 and from Control to Treated - were signs of treatments
beedata1$Site[beedata1$Site == 'C06'] <- 'T106' 
beedata1<-beedata1 %>%
  mutate(Habitat = ifelse(beedata1$Site == "T106", "Treated", Habitat))

# Ratio Data ################################################################################
beedata1$oligRatio <- beedata1$nOligGrowth/beedata1$nTotProvised
beedata1$tstanRatio <- beedata1$nTstanCells/beedata1$nTotProvised
beedata1$monoRatio <- beedata1$nParasite/beedata1$nOligGrowth
beedata1$oligFRatio <- beedata1$nOligF/beedata1$nOligPupae

# Clean data ###############################
beedata1<-as.data.frame(beedata1)
## replace NAs with 0
beedata1[is.na(beedata1)] <- 0
## Add Year
beedata1$Year <- "2020"
## re-org
beedata1 <-beedata1[,c(1,2,19,3:18)]
# Clean Environment #########################################################################
### Get rid of extraneous sets
rm(nestdata)
rm(noligF)
rm(noligM)
rm(noligL)
rm(noligP)
rm(oligFMass)
rm(oligGrowthCells)
rm(oligMass)
rm(oligMMass)
rm(oligLarvMass)
rm(parasiteCells)
rm(totProvCells)
rm(tstanCells)
