# title: "01b.3_beeReproDev_NestContents_dataset.R"
# author: "Jessie Dodge"
# date: "2024-05-17"
# Purpose: This file is used to combine nest cell contents measured 
# within Osmia lignaria nests surveyed in 2020 and 2021  in Boulder County sites
# projects determining landscape effects on  Osmia lignaria parasitism and diet 
# Library ###################################################################################
source(file="01a_libraries.R")
source(file="01b.1_beeReproDev_2020_NestContents_dataset.R")
source(file="01b.2_beeReproDev_2021_NestContents_dataset.R")

# Combine into Master datasheet ###########################################################
beedata <- dplyr::bind_rows(beedata1, beedata2)

# clean DF
#beedata[is.na(beedata)] <- 0 # replaces "NA" with zeros 
## bee larvae from 2021 are only vars with "NAs"- this wasn't because there 
## weren't any though, they just weren't weighed - so leave them as NAs

## Row Names ###################################
# to avoid site name duplication
beedata$Yr<-as.factor(ifelse(beedata$Year == "2020", 20, 21))
beedata$SiteYr <- paste(beedata$Site, beedata$Yr, sep = "-")
beedata <- beedata[,c(21, 1:3,20,4:19)]
beedata <- as.data.frame(beedata)
rownames(beedata) <- beedata[,1]

## Reformat ####################################
beedata[3:5] <- lapply(beedata[3:5], as.factor) # to convert categories to factors
beedata[6:21] <- lapply(beedata[6:21], as.numeric) # to convert all to numbers

# Clean environment
rm(beedata1)
rm(beedata2)
