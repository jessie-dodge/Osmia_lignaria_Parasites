# title: "01d_beeReproDev_Tree_dataset.R"
# author: "Jessie Dodge"
# date: "2024-02-19"
# Purpose: This file is used to organize forest structural measurements
# surveyed in 2020 in Boulder County sites for projects determining landscape effects 
# on  Osmia lignaria parasitism and diet 
# Library #####################################################################################################
source(file="01a_libraries.R")

# Tree CC ###################################################################################################
# Tree Canopy Cover (CC)
ccdata <-read.csv("240222_TreeCC_2020-2021.csv", header = TRUE, stringsAsFactors = FALSE)
# Note: this includes 2021 tree canopy cover as well

# There were some extra photos that were calculated, but were not assigned to sites. Need to delete
ccdata <- ccdata %>% filter(!Site=="#N/A") %>% filter(!ImageName=="NA")%>%
  filter(!Box == "C" & !Box == "D") # don't need to include photos taken from plots C and D, so we can omit them as well

### Calculate Tree CC ###################################
# 1) Calculate Tree CC using mean and SD pixel values
## Tree CC has been calculated in Excel, but want to double check
### 1a) If sky value: 1-(mean/total)
      # If tree value: mean/max
ccdata$TreeCC2<-ifelse(ccdata$Pixel.rep == "Sky", (1-(ccdata$Mean/ccdata$Max)), ccdata$Mean/ccdata$Max)
# Looks the same as Tree CC calculated in Excel

### Clean data ###################################
# 1) Rename sites and change habitats
### Rename site C06 to T106 
ccdata$Site[ccdata$Site == 'C06'] <- 'T106' # signs of treatment
### Rename site C06 to T106 
ccdata$Site[ccdata$Site == 'C103'] <- 'C97' # Same site, just has 2 names 
### Change habitat type for T106
ccdata<-ccdata %>%
  mutate(Habitat = ifelse(ccdata$Site == "T106", "T", Habitat))

# 2) Make a data set for each year
ccdata1 <- filter(ccdata, Year != "2021")
ccdata2 <- filter(ccdata, Year == "2021" | Site == "C05" | Site == "C11" | Site == "T106" | 
                    Site == "H06" | Site == "H09" | Site == "H15" | Site == "T08" | 
                    Site == "T98" | Site == "T99")

### Avg TreeCC by Site ###################################
ccSite1 <- ccdata1 %>%
  group_by(Site, Habitat) %>%
  dplyr::summarise(n = n(),  meanTreeCC = mean(TreeCC2, na.rm=TRUE))

ccSite2 <- ccdata2 %>%
  group_by(Site, Habitat) %>%
  dplyr::summarise(n = n(),  meanTreeCC = mean(TreeCC2, na.rm=TRUE))

# 3) omit unnecessary columns
ccSite1<-ccSite1[-c(3)] 
ccSite2<-ccSite2[-c(3)] 

# Tree BA ###################################################################################################
## 2020 ###################################
# Read data
dbhdata1<-read.csv("230630_2020_TreeDBH.csv", header = TRUE)

## Don't know what partially dead (PD) means in status, but because there's only one, lets change to dead (D)
dbhdata1$Status[dbhdata1$Status == "PD"] <- "D"

### I measured tree circumference at BH - need to convert to DBH
dbhdata1<- 
  dbhdata1 %>% mutate(dhb_m_corrected = ifelse(Collected_by == "JMD", dbh_m/pi, dbh_m))
### filter out saplings
dbhdata1 <- filter(dbhdata1, dhb_m_corrected > 0.10) # dbh greater than 10 cm

### TBA by site #######
# Determine mean tree area by hectare
## 1) calculate individual tree's basal area using the formula: foresters constant (pi / (4 * unit2)) x DBH^2
dbhdata1$dbh_cm <- dbhdata1$dhb_m_corrected *100
dbhdata1$TBA_m2 <- (pi / (4*10000)) * ((dbhdata1$dbh_cm)^2) # metric
dbhdata1$TBA_ft2 <- (pi / (4*144)) * ((dbhdata1$dbh_in)^2) # English

## 2) Multiply tree BA by area measured or the expansion factor. I used a 6.9 m (22.6 ft) radius
### 2a) Metric
#### pi * (6.9^2) = 149.5712 m2 (plot area)
#### hectare / plot area = expansion factor
#### 1 hectare = 10,000 m2 
#### 10,000 m2 / 149.5712 m2 = 66.9 m2/hectare
dbhdata1$TBA_ha <- (dbhdata1$TBA_m2) * (10000/(pi*(6.9^2))) # m2/hectare

### 2b) English (for funsies)
#### pi * (22.6^2) = 1604.6 ft2 (plot area)
#### acre / plot area  = expansion factor 
#### 1 acre = 43,560 ft2
#### 43,560 ft2 / 1604.6ft2 = 27.1 ft2/acre 
dbhdata1$TBA_acre <-(dbhdata1$TBA_ft2) * (43560/(pi*(22.6^2))) # ft2/acre

##### then convert to metric 
######  1ft2 = 0.09290304 m2
###### 1 acre =  0.40468564224 hectares (ha) 
###### 0.09290304 ÷ 0.40468564224 =  0.2295684113865932 m2/ha 
###### 1ft/acre = 0.22956 m2/ha
dbhdata1$TBA_ha2<- dbhdata1$TBA_acre * 0.229568 # this should be the same as TBA_ha. 

## 3) Average by site, habitat, species, and tree status (live or dead)
tba.sppSite <- dbhdata1 %>%
  group_by(Site, Habitat, Species, Status) %>%
  dplyr::summarise(n = n(), mean_BA = mean(TBA_ha))
# should we use mean or sum?

### Clean data ###################################
# Combing species and status
### We don't need density data here (in table above, and omitting this will make it easier to reshape data frame)
### Likewise, we can delete Species and Status because we combined them into one column
tba.sppSite$Species_Status <- paste(tba.sppSite$Species, tba.sppSite$Status, sep = "_")
tba.sppSite<-tba.sppSite[-c(3:5)] 

# to change to wide format
tba.sppSite<-spread(tba.sppSite, Species_Status, mean_BA)

# To average BA just by status (regardless of species)
tba.statSite <-dbhdata1 %>%
  group_by(Site, Habitat, Status) %>%
  dplyr::summarise(n = n(),StatMean_TBA = mean(TBA_ha))

tba.statSite<-tba.statSite[-c(4)] ### Get rid of n (only need for tree density)

# To change to wide format
tba.statSite<-spread(tba.statSite, Status, StatMean_TBA)
#head(StatusBAw)

# To average by total BA (regardless of species or status)
tbaSite <-dbhdata1 %>%
  group_by(Site, Habitat) %>%
  dplyr::summarise(n = n(), TotMean_BA = mean(TBA_ha))
#head(TotalBA)
tbaSite<-tbaSite[-c(3)] # get rid of n
### Don't need to spread - already has one value per site

# To combine all
tbaSite <- merge(tbaSite, tba.sppSite, by=c("Site", "Habitat"))
tbaSite <- merge(tbaSite, tba.statSite, by=c("Site", "Habitat"))

## to rename some variables
names(tbaSite)[names(tbaSite) == "D"] <- "Dead_BA_ha"
names(tbaSite)[names(tbaSite) == "L"] <- "Live_BA_ha"

## omit Habitat columns
tbaSite<-tbaSite[-c(2)] 

## 2021 ###################################
# Read data
dbhdata2<-read.csv("230531_2021_TreeDBH.csv", header = TRUE)

## Change PD to D
dbhdata2$Status[dbhdata2$Status == "PD"] <- "D"

### I measured tree circumference at BH - need to convert to DBH
dbhdata2<- 
  dbhdata2 %>% mutate(dbh_m_corrected = ifelse(Collected_by == "JMD", dbh_m/pi, dbh_m))

### filter out saplings
dbhdata2 <- filter(dbhdata2, dbh_m_corrected > 0.10) # dbh greater than 10 cm

### TBA by site #######
# Determine mean tree area by hectare
## 1) calculate individual tree's basal area
dbhdata2$dbh_cm <- dbhdata2$dbh_m_corrected*100

### check Excel math
dbhdata2<-dbhdata2 %>% mutate_at(vars(dbh_ft), ~ replace(., is.na(.), 0))
dbhdata2$dbh_in.2<- (dbhdata2$dbh_ft * 24) + dbhdata2$dbh_in
dbhdata2$dbh_cm.2 <- dbhdata2$dbh_in.2 * 2.54

dbhdata2$TBA_m2 <- (pi / (4*10000)) * ((dbhdata2$dbh_cm)^2) # metric

## 2) Multiply tree BA by area measured or the expansion factor. I used a 6.9 m (22.6 ft) radius
### 2a) Metric
dbhdata2$TBA_ha <- (dbhdata2$TBA_m2) * (10000/(pi*(6.9^2))) # m2/hectare

## 3) Average by site, habitat, species, and tree status (live or dead)
tba.sppSite2 <- dbhdata2 %>%
  group_by(Site, Habitat, Species, Status) %>%
  dplyr::summarise(n = n(), mean_BA = mean(TBA_ha))

### Clean data ###################################
# Combing species and status
### We don't need density data here (in table above, and omitting this will make it easier to reshape data frame)
### Likewise, we can delete Species and Status because we combined them into one column
tba.sppSite2$Species_Status <- paste(tba.sppSite2$Species, tba.sppSite2$Status, sep = "_")
tba.sppSite2<-tba.sppSite2[-c(3:5)] 

# to change to wide format
tba.sppSite2<-spread(tba.sppSite2, Species_Status, mean_BA)

# To average BA just by status (regardless of species)
tba.statSite2 <-dbhdata2 %>%
  group_by(Site, Habitat, Status) %>%
  dplyr::summarise(n = n(),StatMean_TBA = mean(TBA_ha))

tba.statSite2<-tba.statSite2[-c(4)] ### Get rid of n (only need for tree density)

# To change to wide format
tba.statSite2<-spread(tba.statSite2, Status, StatMean_TBA)
#head(StatusBAw)

# To average by total BA (regardless of species or status)
tbaSite2 <-dbhdata2 %>%
  group_by(Site, Habitat) %>%
  dplyr::summarise(n = n(), TotMean_BA = mean(TBA_ha))
#head(TotalBA)
tbaSite2<-tbaSite2[-c(3)] # get rid of n
### Don't need to spread - already has one value per site

# To combine all
tbaSite2 <- merge(tbaSite2, tba.sppSite2, by=c("Site", "Habitat"))
tbaSite2 <- merge(tbaSite2, tba.statSite2, by=c("Site", "Habitat"))

## to rename some variables
names(tbaSite2)[names(tbaSite2) == "D"] <- "Dead_BA_ha"
names(tbaSite2)[names(tbaSite2) == "L"] <- "Live_BA_ha"

## omit Habitat columns
tbaSite2<-tbaSite2[-c(2)] 

# Merge Tree Data ######################################################################################
treedata1<- merge(ccSite1, tbaSite, by=c("Site"), all=TRUE)
treedata2<- merge(ccSite2, tbaSite2, by=c("Site"), all=TRUE)

## Rename habitats 
treedata1$Habitat <-dplyr::recode(treedata1$Habitat, C = "Control", H = "Burned", T = "Treated")
treedata2$Habitat <-dplyr::recode(treedata2$Habitat, C = "Control", H = "Burned", T = "Treated")

# Combine Years ##############################
## Add year as variable
treedata1$Year <- as.factor("2020")
treedata1<- treedata1[,c(1:2,15,3:14)] # re-org
treedata2$Year <- as.factor("2021")
treedata2<- treedata2[,c(1:2,16,3:15)] # re-org

treedata <- dplyr::bind_rows(treedata1, treedata2)
treedata <- as.data.frame(treedata)

## Clean Data ##################################
## Replace "NA" with 0 
## replace NA's with 0, except for site C106/T106 - missing data for site but there were trees there
treedata[is.na(treedata)] <- 0
treedata[6:16] <- lapply(treedata[6:16] , \(x) x <- 
                           ifelse(x == 0 & treedata$Site == "T106" , NA , x))

## Row Names ###################################
# To overcome duplicate site names
treedata$Yr<-as.factor(ifelse(treedata$Year == "2020", 20, 21))
treedata$SiteYr <- paste(treedata$Site, treedata$Yr, sep = "-")
treedata <- treedata[,c(18, 1:3, 17, 4:16)]
treedata <- as.data.frame(treedata)
rownames(treedata) <- treedata[,1]

## Reformat ####################################
treedata[3:5] <- lapply(treedata[3:5], as.factor) # to convert categories to factors
treedata[6:18] <- lapply(treedata[6:18], as.numeric) # to convert all to numbers

# Clean Environment ##########################################################################################
### Get rid of extraneous sets
rm(ccdata)
rm(ccdata1)
rm(ccdata2)
rm(ccSite1)
rm(ccSite2)
rm(dbhdata1)
rm(dbhdata2)
rm(tba.sppSite)
rm(tba.sppSite2)
rm(tba.statSite)
rm(tba.statSite2)
rm(tbaSite)
rm(tbaSite2)
rm(treedata1)
rm(treedata2)
