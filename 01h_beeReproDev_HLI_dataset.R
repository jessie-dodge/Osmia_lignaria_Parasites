# title: "01h_beeReproDev_HLI_dataset.R"
# author: "Jessie Dodge"
# date: "2024-02-19"
# Purpose: This file is used to calculate HLI for each site surveyed in 
# Boulder County sites sampled in 2020 and 2021 for projects determining landscape effects 
# on Osmia lignaria parasitism and diet

# Library #####################################################################################################
source(file="01a_libraries.R")

# Site Data ###################################################################################################
sitedata<-read.csv("240219_BoCo_sites_2020-2021.csv")

# Clean data #################################################################################################
# rename Site_ID to Site
names(sitedata)[names(sitedata)=="Site_ID"] <- "Site"

# HLI ########################################################################################################
# Heat Load Index (HLI) from McCune and Keon (2002)

# 1)  Transform aspect so max value represents SW and min value represents NE
## To measure HLI, aspect will first need to be transformed (or folded) to account for different facing slopes (NE vs SW)
##   * floded aspect (AKA trasp) = |180 - |Aspect - 225||
##      ** we want 225 (maximum value) to represent SW and 0 (min value) to represent NE
sitedata$trasp <- abs(180 - (abs(sitedata$boco_aspec - 225)))
##  Note: this result is a unitless index of head load - no basis for converting result into 
##  cumulative measure of temperature

# 2) Calculate HLI
## HLI (eq 1) from McCune and Keon (2002)
sitedata <- sitedata %>% mutate(
  HLI = (-1.467) + 1.582 * cos(Lat) * cos(slope) -1 * cos(trasp) * sin(slope) * sin(Lat) - 0.262 * sin(Lat) * sin(slope) + 
    0.607 * sin(trasp) * sin(slope))

## Clean data ##############################################################
## Clean Data ##################################
# select variables you want to keep
sitedata <- sitedata[,c(1:3,9,12,10,13)]

## Row Names ###################################
# to avoid site name duplication
sitedata$Yr<-as.factor(ifelse(sitedata$Year == "2020", 20, 21))
sitedata$SiteYr <- paste(sitedata$Site, sitedata$Yr, sep = "-")
sitedata <- sitedata[c(9,1:3,8,4:7)] #re-org
sitedata <- as.data.frame(sitedata)
rownames(sitedata) <- sitedata[,1]

## Reformat ####################################
sitedata[3:5] <-lapply(sitedata[3:5], as.factor)
sitedata[6:9] <- lapply(sitedata[6:9], as.numeric)
