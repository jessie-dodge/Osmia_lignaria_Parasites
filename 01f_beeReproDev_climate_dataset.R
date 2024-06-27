# title: "01f_beeReproDev_climate_dataset.R"
# author: "Jessie Dodge"
# date: "2024-01-09"
# Purpose: This file is used to organize climate variables downloaded from PRISM for 
# Boulder County sites sampled in 2020 and 2021 for projects determining landscape effects 
# on Osmia lignaria parasitism and diet 

# Data: PRISM Time Series Data
# Locations: 39
# Climate variables: precipitation (ppt),minimum mean temperature (tmin),tmean,tmax
# Spatial resolution: 800 m
# Monthly 1991-2020 Normals
# Dataset: Norm91m
# PRISM day definition: 24 hours ending at 1200 UTC on the day shown
# Grid Cell Interpolation: Off
# Time series generated: 2023-Nov-16
# Details: http://www.prism.oregonstate.edu/documents/PRISM_datasets.pdf

# Library ######################################################################################################
source(file="01a_libraries.R")
# 30-year Climate data ##########################################################################################
## PRISM 30-year normals  ##################################################
normalsdata <- read.csv("PRISM_ppt_tmin_tmean_tmax_30yr_monthly_normals_800m_reorg.csv", header = TRUE)

## Clean data ##############################################################
### Rename vars
normalsdata <- normalsdata %>%
  rename("Site"="Name", "Elevation_m" = "Elevation..m.", "Ann_ppt_mm" = "ppt..mm.", "Ann_tmin_C" = "tmin..degrees.C.", 
         "Ann_tmean_C" ="tmean..degrees.C.", "Ann_tmax_C"= "tmax..degrees.C." )

# rename PC10 to C104
normalsdata$Site[normalsdata$Site == 'PC10'] <- 'C104' 

# Rename site C06 to T106 and from Control to Treated - were signs of treatments
normalsdata$Site[normalsdata$Site== 'C06'] <- 'T106' 

### Check site names
normalsdata %>% pull(Site) %>% unique() %>% sort() 
# can compare to sites from beedata 

## Add Vars ################################################################
### Year ##########################################
## 2020 Sites: C03, C21, H03, H18, T02, T05 (n=6)

## 2021 Sites: C101, C105, C107, C108, C97, H05, H101, H103, H12, H13,
# PC10, T101, T103, T13, T96 (n=15)

## Sites sampled in 2020 & 2021: C05, C11, H06, H09, H15, T08, T106, T98, and T99 (n=9)
# Need to duplicate these sites, one for 2020 and for 2021? 

#ifelse(<condition>, <yes>, ifelse(<condition>, <yes>, <no>))
normalsdata <- normalsdata %>% 
  mutate(Year = ifelse(normalsdata$Site == "C05" | normalsdata$Site == "C11" | normalsdata$Site == "H06" | 
                       normalsdata$Site == "H09" | normalsdata$Site == "H15" | normalsdata$Site == "T08" | 
                       normalsdata$Site == "T106"| normalsdata$Site == "T98" | normalsdata$Site == "T99", list(c(2020, 2021)), # duplicate sites
                       ifelse(normalsdata$Site == "C03"|normalsdata$Site == "C21"| normalsdata$Site == "H03"|
                              normalsdata$Site == "H18"|normalsdata$Site == "T02"| normalsdata$Site == "T05", 2020, 2021))) %>% # 2020 sites
        unnest(Year)  # converts data back from being a list

### Should have 507 obs. --> 39 sites * (12 seasons + 1 annual) = 507

### Habitat ########################################
normalsdata$Habitat <- substr(normalsdata$Site, 1, 1) # makes column of first letter found in Site (Habitat)
normalsdata$Habitat <-dplyr::recode(normalsdata$Habitat, C = "Control", H = "Burned", T = "Treated")

## Subset Annual 30-yr normals ################################################
normalsdata<- normalsdata %>% subset(Date == "Annual")

## Row Names ##################################################################
# to avoid site name duplication
normalsdata$Yr<-as.factor(ifelse(normalsdata$Year == "2020", 20, 21))
normalsdata$SiteYr <- paste(normalsdata$Site, normalsdata$Yr, sep = "-")
normalsdata <- normalsdata[c(13,1,11,10,12,2:9)] #re-org
normalsdata <- normalsdata[,-c(9)]
normalsdata <- as.data.frame(normalsdata)
rownames(normalsdata) <- normalsdata[,1]

## Reformat ###################################################################
normalsdata[1:5] <-lapply(normalsdata[1:5], as.factor)
normalsdata[6:12] <- lapply(normalsdata[6:12], as.numeric)

# 2020 Climate Data #################################################################################
## PRISM month ##################################################################
climdata20 <- read.csv("240307_BoCo_sites_2020_PRISMdata_montly.csv")
## Clean data ##################################################################
### Rename vars
climdata20 <- climdata20 %>%
  rename("Site"="Name", "Elevation_m" = "Elevation..m.", "ppt_mm" = "ppt..mm.", "tmin_C" = "tmin..degrees.C.", 
         "tmean_C" ="tmean..degrees.C.", "tmax_C"= "tmax..degrees.C." )

# copy WellSample col
climdata20$Date2<-climdata20$Date
# separate WellSample2 into 2 cols - Site and Yr
climdata20 <- separate_wider_delim(climdata20, cols = Date2, delim = "-", names = c("Year", "Month"))

## Add Vars ################################################################
climdata20$Habitat <- substr(climdata20$Site, 1, 1) # makes column of first letter found in Site (Habitat)
climdata20$Habitat <- dplyr::recode(climdata20$Habitat, C = "Control", H = "Burned", T = "Treated")

## Subset data #############################################################
## Subset of winter and spring months
climdata20_biann <- climdata20 %>% subset(Month == "01" | Month == "02" | Month == "03" | Month == "04" |Month == "05")

## Subset out months of study (May - July)
climdata20_quart <- climdata20 %>% subset(Date == "2020-05" | Date == "2020-06" | Date == "2020-07")

## Average per Site ##########################################################
climdata20_biann <- climdata20_biann %>% group_by(Site, Habitat, Year) %>%
  summarise_at(c("ppt_mm", "tmin_C", "tmean_C", "tmax_C"), ~ mean(.x)) %>%
  rename("bi_ppt_mm"="ppt_mm", "bi_tmin_C"="tmin_C", "bi_tmean_C"="tmean_C", "bi_tmax_C"="tmax_C")

climdata20_quart <- climdata20_quart %>% group_by(Site, Habitat, Year) %>%
  summarise_at(c("ppt_mm", "tmin_C", "tmean_C", "tmax_C"), ~ mean(.x)) %>%
  rename("qrt_ppt_mm"="ppt_mm", "qrt_tmin_C"="tmin_C", "qrt_tmean_C"="tmean_C", "qrt_tmax_C"="tmax_C")

## Row Names ##################################################################
# to avoid site name duplication
## Biannual data
climdata20_biann$Yr<-"20"
climdata20_biann$SiteYr <- paste(climdata20_biann$Site, climdata20_biann$Yr, sep = "-")
climdata20_biann <- climdata20_biann[c(9,1:3,8,4:7)] #re-org
climdata20_biann <- as.data.frame(climdata20_biann)
rownames(climdata20_biann) <- climdata20_biann[,1]

## Growing quarter
climdata20_quart$Yr<-"20"
climdata20_quart$SiteYr <- paste(climdata20_quart$Site, climdata20_quart$Yr, sep = "-")
climdata20_quart <- climdata20_quart[c(9,1:3,8,4:7)] #re-org
climdata20_quart <- as.data.frame(climdata20_quart)
rownames(climdata20_quart) <- climdata20_quart[,1]

## Reformat ###################################################################
climdata20_biann[1:5] <-lapply(climdata20_biann[1:5], as.factor)
climdata20_biann[6:9] <- lapply(climdata20_biann[6:9], as.numeric)

climdata20_quart[1:5] <-lapply(climdata20_quart[1:5], as.factor)
climdata20_quart[6:9] <- lapply(climdata20_quart[6:9], as.numeric)

# 2021 Climate Data #################################################################################
## PRISM month ##################################################################
climdata21 <- read.csv("240307_BoCo_sites_2021_PRISMdata_montly.csv")
## Clean data ##################################################################
### Rename vars
climdata21 <- climdata21 %>%
  rename("Site"="Name", "Elevation_m" = "Elevation..m.", "ppt_mm" = "ppt..mm.", "tmin_C" = "tmin..degrees.C.", 
         "tmean_C" ="tmean..degrees.C.", "tmax_C"= "tmax..degrees.C." )

# copy Date 
climdata21$Date2<-climdata21$Date
# separate Date2 into 2 cols - Site and Yr
climdata21 <- separate_wider_delim(climdata21, cols = Date2, delim = "-", names = c("Year", "Month"))

## Add Vars ################################################################
climdata21$Habitat <- substr(climdata21$Site, 1, 1) # makes column of first letter found in Site (Habitat)
climdata21$Habitat <- dplyr::recode(climdata21$Habitat, C = "Control", H = "Burned", T = "Treated")

## Subset data #############################################################

## Subset of winter and spring months 
climdata21_biann <- climdata21 %>% subset(Month == "01" | Month == "02" | Month == "03" | Month == "04" |Month == "05")

## Subset out months of study (May - July)
climdata21_quart <- climdata21 %>% subset(Date == "2021-05" | Date == "2021-06" | Date == "2021-07")

## Average per Site ##########################################################
climdata21_biann <- climdata21_biann %>% group_by(Site, Habitat, Year) %>%
  summarise_at(c("ppt_mm", "tmin_C", "tmean_C", "tmax_C"), ~ mean(.x)) %>%
  rename("bi_ppt_mm"="ppt_mm", "bi_tmin_C"="tmin_C", "bi_tmean_C"="tmean_C", "bi_tmax_C"="tmax_C")

climdata21_quart <- climdata21_quart %>% group_by(Site, Habitat, Year) %>%
  summarise_at(c("ppt_mm", "tmin_C", "tmean_C", "tmax_C"), ~ mean(.x)) %>%
  rename("qrt_ppt_mm"="ppt_mm", "qrt_tmin_C"="tmin_C", "qrt_tmean_C"="tmean_C", "qrt_tmax_C"="tmax_C")

## Row Names ##################################################################
# to avoid site name duplication
## Biannual data
climdata21_biann$Yr<-"21"
climdata21_biann$SiteYr <- paste(climdata21_biann$Site, climdata21_biann$Yr, sep = "-")
climdata21_biann <- climdata21_biann[c(9,1:3,8,4:7)] #re-org
climdata21_biann <- as.data.frame(climdata21_biann)
rownames(climdata21_biann) <- climdata21_biann[,1]

## Growing quarter
climdata21_quart$Yr<-"21"
climdata21_quart$SiteYr <- paste(climdata21_quart$Site, climdata21_quart$Yr, sep = "-")
climdata21_quart <- climdata21_quart[c(9,1:3,8,4:7)] #re-org
climdata21_quart <- as.data.frame(climdata21_quart)
rownames(climdata21_quart) <- climdata21_quart[,1]

## Reformat ###################################################################
climdata21_biann[1:5] <-lapply(climdata21_biann[1:5], as.factor)
climdata21_biann[6:9] <- lapply(climdata21_biann[6:9], as.numeric)

climdata21_quart[1:5] <-lapply(climdata21_quart[1:5], as.factor)
climdata21_quart[6:9] <- lapply(climdata21_quart[6:9], as.numeric)

# Merge ###############################################################################################
climdata_biann<-dplyr::bind_rows(climdata20_biann, climdata21_biann)
climdata_quart<-dplyr::bind_rows(climdata20_quart, climdata21_quart)

climdata<- list(normalsdata, climdata_biann, climdata_quart)
climdata <- climdata %>% reduce(left_join, by=c("SiteYr", "Site","Habitat", "Year", "Yr"))
rownames(climdata) <- climdata[,1]
#climdata[,1]<-NULL
# Clean Environment ###################################################################################
### Get rid of extraneous sets
rm(climdata_biann)
rm(climdata_quart)
rm(climdata20)
rm(climdata20_biann)
rm(climdata20_quart)
rm(climdata21)
rm(climdata21_biann)
rm(climdata21_quart)
rm(normalsdata)