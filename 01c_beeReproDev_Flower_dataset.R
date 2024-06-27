# title: "01c_beeReproDev_Flower_dataset.R"
# author: "Jessie Dodge"
# date: "2024-02-19"
# Purpose: This file is used to organize and combine average flower data surveyed 
# in 2020 and 2021 in Boulder County sites for projects determining landscape effects 
# on  Osmia lignaria parasitism and diet 
# Library ####################################################################################################
source(file="01a_libraries.R")

# 2020 Data ##################################################################################################
fpdata1<-read.csv("240219_2020_Floral_data_Plot_ReOrg_Cleaned_v4_JD.csv")

## Clean data #################################################################################################
### Add year column
fpdata1$Year <- as.factor("2020")
fpdata1<-fpdata1[c(1:4,18, 5:17)] # move to 5th column

### Need to cut species that were not flowering between May and June
fpdata1 <- fpdata1 %>% filter(Bloom_Time !="Aug-Oct" & Bloom_Time !="July-Oct" & 
                                  Bloom_Time !="July-Sept" & Bloom_Time !="Just leaves")

### Rename site C06 tp T106 and from Control to Treated - were signs of treatments
fpdata1$Site[fpdata1$Site == 'C06'] <- 'T106' 
fpdata1<-fpdata1 %>%
  mutate(Habitat = ifelse(fpdata1$Site == "T106", "Treated", 
                          ifelse(fpdata1$Habitat == "High severity", "Burned", Habitat)))

### Change vars to numeric
fpdata1$NumBlooms<-as.numeric(fpdata1$NumBlooms)
fpdata1$NumStems<-as.numeric(fpdata1$NumStems)

### Average Number of Stems by Species per Site
fp.spp <- fpdata1 %>% group_by(Site, Species2) %>%
  summarise_at(c("NumStems"), ~ mean(.x)) 

## Species DF ##################################################################################################
# 1) Re-Organize to make into Species Matrix
# Need to reorganize dataframe in order to determine community metrics (diversity, richness,
#   and abundance/density)
site.fp<-spread(fp.spp, key=Species2, value = NumStems)
##### fact - this is WAY easier than Index Matching in Excel

# replace NAs with 0
site.fp[is.na(site.fp)] <- 0

# 2) Get rid of "NA" species column
site.fp<-site.fp[,-c(59)]
## Can check using >names(site.fp)

# 3) Make sure all flower species read as numbers (not integers)
#str(site.fp2)
site.fp[2:58] <- lapply(site.fp[2:58], as.numeric)

## Site Groups ################################################################################################
site.grp <- unique(fpdata1[c(3:5)])

## Community Analysis #########################################################################################
# Change Sites to rownames
site.fp <-as.data.frame(site.fp)
rownames(site.fp) <- site.fp[,1]
site.fp[,1] <- NULL

### Index measures #################################
stemdiv<-diversity(site.fp) #Shannon's diversity
stemrich <-(rowSums(site.fp>0)) #Richness
stemabun <-(rowSums(site.fp)) 

stemdata1<-cbind(site.grp, stemdiv)
stemdata1<-cbind(stemdata1, stemrich)
stemdata1<-cbind(stemdata1, stemabun)

# 2021 Data ###################################################################################################
fpdata2<-read.csv("240223_2021_floralData_Plot_JD.csv")

## Clean data ##################################################################################################
### Add year column
fpdata2$Year <- as.factor("2021")
fpdata2<-fpdata2[c(1:3, 15, 4:14)] # move to 5th column

### Species to cut: COPA3, LEPID, GALIU, UnkFuzz2, CLRO5, NOFE3, UnkW
# These species were "belly plants" and not likely to be pollinated by bees
fpdata2 <- fpdata2 %>% filter(Code != "COPA3"& Code != "LEPID"& Code != "GALIU"& Code != "UnkFuzz2"& 
                                Code != "CLRO5"& Code != "NOFE3"& Code != "UnkW")

## Note: can check if successful using code >fpdata2 %>% pull(Code) %>% unique() %>% sort()
## Can also check for spelling errors or duplicate species using code >fpdata2 %>% pull(Species2) %>% unique() %>% sort()

### Change vars to numeric (from integers)
fpdata2$NumBlooms<-as.numeric(fpdata2$NumBlooms)
fpdata2$NumStems<-as.numeric(fpdata2$NumStems)

### Deal with rows with "NA" NumStems
# There were some blanks in NumBlooms & NumStems. 
# For NumStems, only Physaria bellii (PHBE2) from site H12 was blank (forgot to count?)
# is row 789.
# by changing NA to 0, Burned PHBE2 mean stem = 2.818182 
#fpdata2[is.na(fpdata2)] <- 0
# by omitting row, mean is 3.100000. may need to omit rows with missing data
fpdata2<-fpdata2[-c(789),]

### Stem & Bloom abundance ################################################################
### Average Number of Stems by Species per Site
fp2.spp <- fpdata2 %>% group_by(Site, Species2) %>%
  summarise_at(c("NumStems"), ~ mean(.x)) 

fpb2.spp <- fpdata2 %>% group_by(Site, Species2) %>%
  summarise_at(c("NumBlooms"), ~ mean(.x)) 

## Species DF ##################################################################################################
# 1) Re-Organize to make into Species Matrix
# Need to reorganize dataframe in order to determine community metrics (diversity, richness,
#   and abundance/density)
site.fp2<-spread(fp2.spp, key=Species2, value = NumStems)
site.fpb2<-spread(fpb2.spp, key=Species2, value = NumBlooms)
##### fact - this is WAY easier than Index Matching in Excel

# 2) replace NAs with 0
site.fp2[is.na(site.fp2)] <- 0
site.fpb2[is.na(site.fpb2)] <- 0

# 3) Make sure all flower species read as numbers (not integers)
#str(site.fp2)
site.fp2[2:36] <- lapply(site.fp2[2:36], as.numeric)
site.fpb2[2:36] <- lapply(site.fpb2[2:36], as.numeric)

#4) Make sure there are no species with 0 stems/ 0 blooms
colSums(site.fp2[,-1]) == 0
colSums(site.fpb2[,-1]) == 0

## Site Groups #################################################################################################
site.grp2 <- unique(fpdata2[c(2:4)])

## Community Analysis ##########################################################################################
# Change Sites to rownames
site.fp2 <-as.data.frame(site.fp2)
rownames(site.fp2) <- site.fp2[,1]
site.fp2[,1] <- NULL

### Index measures #################################
### stems
stemdiv <-diversity(site.fp2) #Shannon's diversity
stemrich <-(rowSums(site.fp2>0)) #Richness
stemabun <-(rowSums(site.fp2)) 

stemdata2<-cbind(site.grp2, stemdiv)
stemdata2<-cbind(stemdata2, stemrich)
stemdata2<-cbind(stemdata2, stemabun)

### blooms
bloomdiv<-diversity(site.fpb2[c(2:36)]) #Shannon's diversity
bloomrich <-(rowSums(site.fpb2[c(2:36)]>0)) #Richness
bloomabun <-(rowSums(site.fpb2[c(2:36)])) 

bloomdata2<-cbind(site.grp2, bloomdiv)
bloomdata2<-cbind(bloomdata2, bloomrich)
bloomdata2<-cbind(bloomdata2, bloomabun)

### Merge both
stemdata2<-merge(stemdata2, bloomdata2, by = c("Site", "Habitat", "Year"))

# Combine into Master datasheet ###########################################################
stemdata <- dplyr::bind_rows(stemdata1, stemdata2)
stemdata <- as.data.frame(stemdata)

# clean DF
#stemdata[is.na(stemdata)] <- 0 # replaces "NA" with zeros 
## stem larvae from 2021 are only vars with "NAs"- this wasn't because there 
## weren't any though, they just weren't weighed - so leave them as NAs

## Row Names ###################################
# To overcome duplicate site names
stemdata$Yr<-as.factor(ifelse(stemdata$Year == "2020", 20, 21))
stemdata$SiteYr <- paste(stemdata$Site, stemdata$Yr, sep = "-")
stemdata <- stemdata[,c(11, 1:3, 10, 4:9)]
stemdata <- as.data.frame(stemdata)
rownames(stemdata) <- stemdata[,1]

## Reformat ####################################
stemdata[3:5] <- lapply(stemdata[3:5], as.factor) # to convert categories to factors
stemdata[6:11] <- lapply(stemdata[6:11], as.numeric) # to convert all to numbers

# Clean Environment ##########################################################################################
### Get rid of extraneous sets
rm(bloomdata2)
rm(fp.spp)
rm(fp2.spp)
rm(fpb2.spp)
rm(fpdata1)
rm(fpdata2)
rm(site.fp)
rm(site.fp2)
rm(site.fpb2)
rm(site.grp)
rm(site.grp2)
rm(stemdata1)
rm(stemdata2)
rm(bloomdiv)
rm(bloomrich)
rm(bloomabun)
rm(stemdiv)
rm(stemrich)
rm(stemabun)

