# title: "01g_beeReproDev_degreeDay_dataset.R"
# author: "Jessie Dodge"
# date: "2024-05-20"
# Purpose: This file is used to calculate degree days accumulated for bees while in 
# Boulder County sites sampled in 2020 and 2021 for projects determining landscape effects 
# on Osmia lignaria parasitism and diet. Data was downloaded from PRISM. 

# Library ######################################################################################################
source(file="01a_libraries.R")

# 2020 Data ####################################################################################################
#dd20data <- read.csv("2020_PRISM_tmean_stable_4km_0512_0727.csv", header = TRUE)
dd20data <- read.csv("240522_2020_accumulatedDegreeDays.csv", header = TRUE)

# Note: I couldn't figure out how to calculate ADD in R (adding previous rows together in same column that 
# you're creating), and I ran short on time. Therefore, I just did it in excel. I left the code I started under 
# 2021 Data - Calculating degree days accumulated. 

## Data clean ##################################
# Separate Date into month, day, year
dd20data <- separate(dd20data, col = Date, into = c("Month", "Day", "Year"), sep = "/")

## 2020 bee nest boxes ########################
### Period: 05/12/20 - 07/27/20 (77 days)
# C03- placed: 05/12/20, collected: 07/02/20
# C05- placed: 05/12/20, collected: 07/03/20
# H03- placed: 05/12/20, no collections
# H06- placed: 05/12/20, collected 07/07/20
# T02- placed: 05/12/20, collected 07/02/20
# T05- placed: 05/12/20, 1 nest collected 07/26/20, rest collected 07/01/20
# T08- placed: 05/12/20, 2 nests collected: 07/26/20 (5 pupae), rest collected 7/3/20 (all larvae)
# T99- placed: 05/12/20, 1 nest collected 7/26/20, rest collected 7/7/20
# C06- placed: 05/13/20, collected: 07/08/20
# C11- placed: 05/13/20, 1 nest collected 07/10/20, rest collected: 07/27/20
# C21- placed: 05/13/20, collected 07/09/2020
# H09- placed: 05/13/20, collected 07/08/2020
# H15- placed: 05/13/20, 1 nest collected 07/27/20 (larvae), rest collected 07/10/20
# H18- placed: 05/13/20, 1 nest collected 07/09/20 (pupae), rest collected: 07/27/20 (pupae)
# T98- placed: 05/13/20, 1 nest collected 07/10/20 (larvae), rest collected 07/27/20

# If using Excel calculated ADD
## Only need max value of ADD, except for sites T05 and T08 -
### Because ~25% of brood collected late in T05 and ~40% collected late in T08,
### I am going to try and use Mean ADD between early and late collections and see 
### what results look like.
adddata1 <- 
  dd20data %>% group_by(Site) %>%
    filter(ifelse(Site == "T05" | Site == "T08", FieldDay == 64, FieldDay == max(FieldDay)))


# 2021 Data ####################################################################################################
#dd21data <- read.csv("2021_PRISM_tmean_stable_4km_0524_0701.csv", header = TRUE)
dd21data <- read.csv("240522_2021_accumulatedDegreeDays.csv", header = TRUE)

## Data clean ##################################
# Separate Date into month, day, year
dd21data <- separate(dd21data, col = Date, into = c("Month", "Day", "Year"), sep = "/")

## 2021 bee nest boxes ########################
### Period: 05/24/21 - 07/01/21 (39 days)
# C11 - placed: 5/24/21, collected 6/28/21
# C97 - placed: 5/24/21, collected 6/28/21
# H15 - placed: 5/24/21, collected 6/28/21
# PC10 - placed: 5/24/21, collected 6/28/21
# T13 - placed: 5/24/21, collected 6/28/21
# T96 - placed: 5/24/21, collected 6/28/21
# T98 - placed: 5/24/21, collected 6/28/21
# C105 - placed: 5/25/21, collected 6/29/21
# H09 - placed: 5/25/21, collected 6/29/21
# H12 - placed: 5/25/21, collected 6/29/21
# H13 - placed: 5/25/21, collected 6/29/21
# T101 - placed: 5/25/21, collected 6/29/21
# T103 - placed: 5/25/21, collected 6/29/21
# T106 - placed: 5/25/21, collected 6/29/21
# C05 - placed: 5/26/21, collected 6/30/21
# C107 - placed: 5/26/21, collected 6/30/21
# H05 - placed: 5/26/21, collected 6/30/21
# H06 - placed: 5/26/21, collected 6/30/21
# H101 - placed: 5/26/21, collected 6/30/21
# H103 - placed: 5/26/21, collected 6/30/21
# T08 - placed: 5/26/21, collected 6/30/21
# C101 - placed: 5/27/21, collected 7/01/21
# C108 - placed: 5/27/21, collected 7/01/21
# T99 - placed: 5/27/21, collected 7/01/21

#Calculating ADD
# Note1: O. lignaria require at least 5 degree days for growth (CITAION NEEDED)
# Note2: There is a package called climatrends that calculates Growing degree-days
# (different from acumulated degree days?), but need min and max temps

#ifelse(<condition>, <yes>, ifelse(<condition>, <yes>, <no>))

# Add Degree days added
#dd21data<-dd21data %>% group_by(Site) %>% mutate(DD_added = ifelse(tmean_C > 5, tmean_C - 5, 0))

# Calculate Accumulated Degree Days
#dd21data<-
#  dd21data %>% group_by(Site) %>% arrange(Date) %>% arrange(Site) %>%
#    mutate(DD = ifelse(row_number() == 1 & DD_added == 0, 0, 
#                            ifelse(row_number() == 1 & DD_added > 0, 1, 
#                                   ifelse(row_number() == 2, DD_added + 1, # will not work if there are 0 DD_added
#                                      accumulate(DD_added, ~ .x-1 + .y))))) # close, but not quite right

# If using Excel calculated ADD
## Only need max value of ADD - all bees in 2021 were left in the field for 36 field days
adddata2 <- dd21data %>% subset(FieldDay == 36)

# Merge ###############################################################################################
adddata<-dplyr::bind_rows(adddata1, adddata2)

## Clean Data ##################################
# select variables you want to keep
adddata <- adddata[,c(2:5,11:12,14,16:17)]
# replace "NA" in 2021 box collections with 2021 bee collections
adddata$BoxesCollected <- ifelse(is.na(adddata$BoxesCollected), adddata$BeesCollected, adddata$BoxesCollected)
# Add Habitat
adddata$Habitat <- substr(adddata$Site, 1, 1)
adddata$Habitat <-dplyr::recode(adddata$Habitat, C = "Control", P = "Control", H = "Burned", T = "Treated")
## Row Names ###################################
# to avoid site name duplication
adddata$Yr<-as.factor(ifelse(adddata$Year == "2020", 20, 21))
adddata$SiteYr <- paste(adddata$Site, adddata$Yr, sep = "-")
adddata <- adddata[c(12,1,10,5,11,2:4,6:9)] #re-org
adddata <- as.data.frame(adddata)
rownames(adddata) <- adddata[,1]

## Reformat ####################################
adddata[3:5] <-lapply(adddata[3:5], as.factor)
adddata[9:12] <- lapply(adddata[9:12], as.numeric)

# Clean Environment ###################################################################################
### Get rid of extraneous sets
rm(adddata1)  
rm(adddata2)
rm(dd20data)
rm(dd21data)
