# title: "01e_beeReproDev_EVT_dataset.R"
# author: "Jessie Dodge"
# date: "2024-01-04"
# Purpose: This file is used to organize Environmental Vegetation Types (EVT) downloaded from LANDFIRE for 
# Boulder County sites and spatially joined to 500 m buffers around both Osmia lignaria sites sampled in 
# 2020 and 2021 using ArcMap Pro for projects determining landscape effects 
# on  Osmia lignaria parasitism and diet 
# Library #############################################################################################
source(file="01a_libraries.R")
# Data files ##########################################################################################
## 2020
evt20data <- read.csv("231116_BoCo2020BuffTsj_Tab.csv")
## 2021
evt21data <- read.csv("231116_BoCo2021BuffTsj_Tab.csv")

## Clean data ##############################################################
### Rename vars
#### 2020
### Rename site C06 to T106 and from Control to Treated - were signs of treatments
evt20data$Site_ID[evt20data$Site_ID== 'C06'] <- 'T106' 
evt20data<-evt20data %>%
  mutate(Habitat = ifelse(evt20data$Site_ID == "T106", "Treated", 
                          ifelse(evt20data$Habitat == "High severity", "Burned", Habitat)))
#### 2021
# rename PC10 to C104
evt21data$Site_ID[evt21data$Site_ID == 'PC10'] <- 'C104' 

# Summarize cells within each EVT per site buffer
## 2020
evt20.sum <- evt20data %>% group_by(Site_ID, EVT_NAME) %>% 
  #dplyr::summarise_at(c("Join_Count", "Shape_Length", "Shape_Area"), sum, na.rm=T)
  dplyr::summarise_at(c("Join_Count"), sum, na.rm=T)
## 2021
evt21.sum <- evt21data %>% group_by(Site_ID, EVT_NAME) %>% 
  #dplyr::summarise_at(c("Join_Count", "Shape_Length", "Shape_Area"), sum, na.rm=T)
  dplyr::summarise_at(c("Join_Count"), sum, na.rm=T)
# Now, we can omit EVTs with less than 5 cells per site buffer
evt20.sum <- filter(evt20.sum, Join_Count >= 5)
evt21.sum <- filter(evt21.sum, Join_Count >= 5)

# Merge into EVT Dataframe ############################################################################
## Add variable for year
evt20.sum$Year <- "2020"
evt21.sum$Year <- "2021"
## Merge
evtsumdata<-dplyr::bind_rows(evt20.sum, evt21.sum)
## Rename Site_ID to Site
evtsumdata<-evtsumdata %>% rename("Site" = "Site_ID")

## To overcome site name duplicates
evtsumdata$Yr<-as.factor(ifelse(evtsumdata$Year == "2020", 20, 21))
evtsumdata$SiteYr <- paste(evtsumdata$Site, evtsumdata$Yr, sep = "-")
## Add Habitat variable
evtsumdata$Habitat <- substr(evtsumdata$Site, 1, 1)
evtsumdata$Habitat <-dplyr::recode(evtsumdata$Habitat, C = "Control", P = "Control", H = "Burned", T = "Treated")
## Reorganize 
evtsumdata <- evtsumdata[c(6,1,7,4:5,2:3)]

# EVT Key Attributes ###########################################################
# Break EVT_NAMES into broader categories, riffing off of EVT key attributes
evtsumdata <- evtsumdata %>% 
  mutate(EVT_Attribute = case_when(EVT_NAME == "Rocky Mountain Lodgepole Pine Forest"| 
                                    EVT_NAME == "Rocky Mountain Subalpine Dry-Mesic Spruce-Fir Forest and Woodland"|
                                    EVT_NAME == "Southern Rocky Mountain Dry-Mesic Montane Mixed Conifer Forest and Woodland"|
                                    EVT_NAME == "Southern Rocky Mountain Mesic Montane Mixed Conifer Forest and Woodland"|
                                    EVT_NAME == "Southern Rocky Mountain Pinyon-Juniper Woodland"|
                                    EVT_NAME == "Southern Rocky Mountain Ponderosa Pine Savanna"|
                                    EVT_NAME == "Southern Rocky Mountain Ponderosa Pine Woodland" 
                                      ~ "Coniferous_Forest", 
                                  EVT_NAME == "Western Cool Temperate Pasture and Hayland" 
                                      ~ "Crops",
                                  EVT_NAME == "Rocky Mountain Aspen Forest and Woodland" |
                                    EVT_NAME == "Rocky Mountain Lower Montane-Foothill Riparian Woodland" 
                                      ~ "Deciduous_Forest", 
                                  EVT_NAME == "Developed-Low Intensity" |
                                    EVT_NAME == "Developed-Roads" |
                                    EVT_NAME == "Western Cool Temperate Developed Evergreen Forest"|
                                    EVT_NAME == "Western Cool Temperate Urban Evergreen Forest" |
                                    EVT_NAME == "Western Cool Temperate Urban Mixed Forest" |
                                    EVT_NAME == "Western Cool Temperate Urban Deciduous Forest" |
                                    EVT_NAME == "Western Cool Temperate Developed Herbaceous" |
                                    EVT_NAME == "Western Cool Temperate Urban Herbaceous" |
                                    EVT_NAME == "Western Cool Temperate Urban Shrubland" 
                                      ~ "WUI",
                                    EVT_NAME == "Rocky Mountain Cliff Canyon and Massive Bedrock" 
                                      ~ "Barren",
                                  #EVT_NAME == "Rocky Mountain Alpine-Montane Wet Meadow" |
                                  #  EVT_NAME == "Rocky Mountain Subalpine-Montane Mesic Meadow" 
                                  #    ~ "Meadow"
                                  TRUE ~ "Rangeland"))
# Coniferous Forest - 
# "Rocky Mountain Lodgepole Pine Forest", 
# "Rocky Mountain Subalpine Dry-Mesic Spruce-Fir Forest and Woodland",
# "Southern Rocky Mountain Dry-Mesic Montane Mixed Conifer Forest and Woodland",
# "Southern Rocky Mountain Mesic Montane Mixed Conifer Forest and Woodland",
# "Southern Rocky Mountain Pinyon-Juniper Woodland",
# "Southern Rocky Mountain Ponderosa Pine Savanna",
# "Southern Rocky Mountain Ponderosa Pine Woodland",

# Crops- 
# "Western Cool Temperate Pasture and Hayland"

# # Deciduous Forest - 
# "Rocky Mountain Aspen Forest and Woodland"
# "Rocky Mountain Lower Montane-Foothill Riparian Woodland"
# 
# # WUI - Include:
#  Developed - Low Intensity, Developed-Coniferous Forest, Dev. Deciduous Forest, Dev Grassland $ Dev shrubland
# "Developed-Low Intensity"
# "Western Cool Temperate Developed Evergreen Forest"
# "Western Cool Temperate Urban Evergreen Forest"
# "Western Cool Temperate Urban Mixed Forest"
# "Western Cool Temperate Urban Deciduous Forest"
# "Western Cool Temperate Developed Herbaceous"
# "Western Cool Temperate Urban Herbaceous"
# "Western Cool Temperate Urban Shrubland" 
# 
# # Barren - 
# "Developed-Roads"
# "Rocky Mountain Cliff Canyon and Massive Bedrock"

# # Rangeland -
#   Include: Grasslands, forblands, meadows, and Sagebrush Steppe
# "Great Basin & Intermountain Introduced Perennial Grassland and Forbland",
# "Interior Western North American Temperate Ruderal Grassland",
# "Southern Rocky Mountain Montane-Subalpine Grassland",
# "Western Great Plains Foothill and Piedmont Grassland"
# "Rocky Mountain Alpine-Montane Wet Meadow",
# "Rocky Mountain Subalpine-Montane Mesic Meadow"
# "Inter-Mountain Basins Montane Sagebrush Steppe",
# "Rocky Mountain Gambel Oak-Mixed Montane Shrubland",
# "Rocky Mountain Lower Montane-Foothill Shrubland"

# Meadow (if not included in rangeland)
# "Rocky Mountain Alpine-Montane Wet Meadow",
# "Rocky Mountain Subalpine-Montane Mesic Meadow"

# Organize DF ##################################################################

# Re-Organize DF into wide format so that EVT are columns
evtdata<-spread(evtsumdata, key=EVT_Attribute, value = Join_Count)

evtdata[is.na(evtdata)] <- 0 # replace NA with 0
# To alphabetize columns
#evtdata <- evtdata %>% dplyr::select(SiteYr, Site, Habitat, Year, Yr, sort(names(.)))

## Average per Site ##########################################################
evtdata <- evtdata %>% group_by(SiteYr, Site, Habitat, Year, Yr) %>%
  summarise(across(c(Barren:WUI), sum))

# Make sure sum of EVT groups match sum(evtsumdata$Join_Count)
# evtdata%>% summarise(across(c(Barren:WUI), sum))

## Row Names #################################################################
evtdata[1:5] <- lapply(evtdata[1:5], as.factor) # make sure vars are numeric
evtdata[6:11] <- lapply(evtdata[6:11], as.numeric) # make sure vars are numeric
evtdata<-as.data.frame(evtdata) # by alphabetizing families, data frame was turned into a tibble
## this code turn the data bake into a data frame
rownames(evtdata) <- evtdata[,1] # name rows by sites
#evtdata[,1] <- NULL # gets rid of site names (don't need them now!)

## Relativize data ###########################################################
evtdata.rel <- decostand(evtdata[,-c(1:5)], method = "total") 
evtdata.rel<-cbind(evtdata[c(1:5)], evtdata.rel)

# Community Index data ###############################################################################
# Diversity, richness, and abundance
EVT_abund <-(rowSums(evtdata[,-c(1:5)])) # Note: relativized abundance should equal 1
EVT_H<-diversity(evtdata.rel[,-c(1:5)]) #Shannon's diversity
EVT_rich <-(rowSums(evtdata.rel[,-c(1:5)]>0)) #Richness without empty sites
#EVT_J<-evenness(evtdata.rel[,-c(1:4)]) #Pielou's evenness
EVT_J <- EVT_H/log(EVT_rich) # manual method

# Combine into Master datasheet ########################################################################
evtdata.rel<-cbind(evtdata.rel, EVT_abund)
evtdata.rel<-cbind(evtdata.rel, EVT_H)
evtdata.rel<-cbind(evtdata.rel, EVT_rich)
evtdata.rel<-cbind(evtdata.rel, EVT_J)

# ReOrg ################################################################################################
evtdata.rel <- evtdata.rel[c(1:5, 12:15, 6:11)]
evtdata.rel$EVT_J <-gsub("NaN", 0, evtdata.rel$EVT_J) # relplaces NaN with 0

# Clean Environment ##########################################################################################
### Get rid of extraneous sets
rm(evt20.sum)
rm(evt20data)
rm(evt21.sum)
rm(evt21data)
rm(evtdata)
#rm(evtdata.rel)
rm(evtsumdata)
rm(EVT_abund)
rm(EVT_H)
rm(EVT_rich)
rm(EVT_J)
