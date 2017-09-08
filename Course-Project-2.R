# Coursera Data Science Specialization Course: Reproducible Resarch
# Course Project 2
# ########################################################################################
# Assignment
# 
# The basic goal of this assignment is to explore the NOAA Storm Database and answer some
# basic questions about severe weather events. You must use the database to answer the 
# questions below and show the code for your entire analysis. Your analysis can consist of
# tables, figures, or other summaries. You may use any R package you want to support your 
# analysis.
# 
# Questions
# 
# Your data analysis must address the following questions:
#   
# 1. Across the United States, which types of events (as indicated in the EVTYPE variable) 
# are most harmful with respect to population health? 
# 2. Across the United States, which types of events have the greatest economic consequences?
# 
# Consider writing your report as if it were to be read by a government or municipal manager 
# who might be responsible for preparing for severe weather events and will need to prioritize
# resources for different types of events. However, there is no need to make any specific 
# recommendations in your report.
#
########################################################################################
# Required Sections
## Title - briefly summarize report
## Synopsis - at most ten sentences describing and summarizing analysis
## Data Processing - words and code
## Results
## No more than 3 figures
## figures need title, axis labels

########################################################################################
# Data Processing
library(tidyverse)

## Data codebook, source url, download, and import

### Codebook: 
"https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf"

### Discussion Forum Notes
"https://www.coursera.org/learn/reproducible-research/discussions/weeks/4/threads/IdtP_JHzEeaePQ71AQUtYw"
 "According to NOAA the data recording start from Jan. 1950. At that time they recorded
  one event type, tornado. They add more events gradually and only from Jan. 1996 they
  start recording all events type. Since our objective is comparing the effects of different
  weather events, do we need to include all years, even it has only single event type?"

### Source re handling PROPDMEXP, CROPDMGEXP
"https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html"

### Data download
#url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
#download.file(url, destfile = "StormData.csv")
stormData <- read.csv("StormData.csv", stringsAsFactors = FALSE)

## Review data set for structure, variable class, etc.
as.tbl(stormData)
glimpse(as.tbl(stormData))
summary(stormData)

## Treat downloaded data as read-only
stormData1 <- stormData

## Change event begin date varaiable class from character to date
stormData1$BGN_DATE <- as.Date(stormData$BGN_DATE, format = "%m/%d/%Y")

## Change event begin date varaiable class from character to date
stormData2 <- stormData1 %>% filter(BGN_DATE >= "1996-01-01")

## Change event type variable from character to factor
stormData2$EVTYPE   <- factor(stormData2$EVTYPE)
stormData2$PROPDMGEXP   <- factor(stormData2$PROPDMGEXP)
stormData2$CROPDMGEXP   <- factor(stormData2$CROPDMGEXP)
#summary(stormData2)

## Rename important event type to conform to Codebook
stormData2$EVTYPE[stormData2$EVTYPE=="HURRICANE/TYPHOON"] <- "HURRICANE"
stormData2$EVTYPE[stormData2$EVTYPE=="TYPHOON"] <- "HURRICANE"
stormData2$EVTYPE[stormData2$EVTYPE=="STORM SURGE"] <- "STORM SURGE/TIDE"
stormData2$EVTYPE[stormData2$EVTYPE=="TSTM WIND"] <- "THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="TSTM WIND/HAIL"] <- "THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="MARINE TSTM WIND"] <- "MARINE THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="WILD/FOREST FIRE"] <- "WILDFIRE"
stormData2$EVTYPE[stormData2$EVTYPE=="FOG"] <- "DENSE FOG"
stormData2$EVTYPE[stormData2$EVTYPE=="URBAN/SML STREAM FLD"] <- "FLOOD"
stormData2$EVTYPE[stormData2$EVTYPE=="WINTER WEATHER/MIX"] <- "WINTER WEATHER"
stormData2$EVTYPE[stormData2$EVTYPE=="HEAVY SURF/HIGH SURF"] <- "HIGH SURF"
stormData2$EVTYPE[stormData2$EVTYPE=="TSTM WIND (G45)"] <- "THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="TSTM WIND (G40)"] <- "THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="STRONG WINDS"] <- "HIGH WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="FREEZING RAIN"] <- "SLEET"
stormData2$EVTYPE[stormData2$EVTYPE=="EXTREME WINDCHILL TEMPERATURES"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="COLD/WIND CHILL"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="EXTREME COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="EXTREME WINDCHILL"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="RECORD COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="UNSEASONABLY COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="Cold"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="UNUSUALLY COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="SNOW"] <- "HEAVY SNOW"
stormData2$EVTYPE[stormData2$EVTYPE=="Snow"] <- "HEAVY SNOW"
stormData2$EVTYPE[stormData2$EVTYPE=="EXCESSIVE SNOW"] <- "HEAVY SNOW"
stormData2$EVTYPE[stormData2$EVTYPE=="WIND"] <- "HIGH WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="Heavy Rain"] <- "HEAVY RAIN"
stormData2$EVTYPE[stormData2$EVTYPE=="RECORD RAINFALL"] <- "HEAVY RAIN"
stormData2$EVTYPE[stormData2$EVTYPE=="RECORD WARMTH"] <- "HEAT"
stormData2$EVTYPE[stormData2$EVTYPE=="UNUSUAL WARMTH"] <- "HEAT"
stormData2$EVTYPE[stormData2$EVTYPE=="RECORD HEAT"] <- "HEAT"
stormData2$EVTYPE[stormData2$EVTYPE=="UNSEASONABLY WARM"] <- "HEAT"
stormData2$EVTYPE[stormData2$EVTYPE=="Winter Weather"] <- "WINTER WEATHER"
stormData2$EVTYPE[stormData2$EVTYPE=="FREEZE"] <- "FROST/FREEZE"
stormData2$EVTYPE   <- factor(stormData2$EVTYPE)
summary(stormData2$EVTYPE)


## For 50 States Only use stormData[(stormData$STATE %in% state.abb),]

## Mutate economic damage variables to common numeric/dollar basis, incorporating exponent variables
## Rescaled to Millions of Dollars ($000,000)
stormData3 <- stormData2 %>%
  mutate(prop_damage = ifelse(PROPDMGEXP == "", PROPDMG*10^-6, 
    ifelse(PROPDMGEXP == "B", PROPDMG*10^3, 
    ifelse(PROPDMGEXP == "M", PROPDMG*10^0, 
    ifelse(PROPDMGEXP == "K", PROPDMG*10^-3,
    ifelse(PROPDMGEXP ==  0,   PROPDMG*10^-5, PROPDMG)))))) %>%
  mutate(crop_damage = ifelse(CROPDMGEXP == "", CROPDMG*10^-6, 
    ifelse(CROPDMGEXP == "B", CROPDMG*10^3, 
    ifelse(CROPDMGEXP == "M", CROPDMG*10^0, 
    ifelse(CROPDMGEXP == "K", CROPDMG*10^-3,
    ifelse(CROPDMGEXP ==  0,  CROPDMG*10^-5, CROPDMG)))))) 


## Checking & Correcting Entries
# Incorrect exponent - per comments
stormData3 %>% filter(prop_damage == 1.15e+05)
stormData3$prop_damage[stormData3$prop_damage == 1.15e+05] <- 115
# stormData3 %>% filter(crop_damage == 1.51e+03) # Katrina
# stormData3 %>% filter(prop_damage == 3.13e+04) # Katrina
summary(stormData3)


## Create total impact variables; reshape to tidy data
stormData4 <- stormData3  %>%
  mutate(total_econ  = prop_damage + crop_damage) %>%
  mutate(total_human = FATALITIES + INJURIES) %>%
  rename(fatalities = FATALITIES) %>% rename(injuries = INJURIES) %>%
  gather("human_events", "human_count", fatalities:injuries) %>%
  gather("economic_events", "dollars", prop_damage, crop_damage) %>%
  select(EVTYPE, BGN_DATE, human_events, human_count, total_human, economic_events, dollars, total_econ)
head(stormData4)  
  
# Question 1
## Event vs. Population Health: Faceted Boxplots
stormData4 %>% filter(total_human > 201) %>%
  ggplot(aes(x = EVTYPE, y = human_count)) + geom_boxplot() + 
    facet_wrap(~human_events, scales = "free") + scale_y_log10() +
  labs(title = "Population Health Impact", x = "Event Type", y = "Number")

## Event vs. Population Health: Table
stormTable1 <- stormData4 %>% filter(total_human > 201) %>%
  group_by(EVTYPE, human_events) %>%
  summarise(median_impact = median(human_count, na.rm = TRUE), 
            max_impact = max(human_count)) %>%
  arrange(desc(median_impact)) 
stormTable1
  
# Question 2: Event Type vs. Economic Damages
## Event Type vs Property Damage: Top 4-7
stormData4 %>% filter(total_econ > 2*10^3) %>%
  ggplot(aes(x = EVTYPE, y = dollars, col = economic_events)) + geom_boxplot() + 
   scale_y_log10() +
    labs(title = "Economic Impact", x = "Event Type", y = "$ Millions")

stormTable2 <- stormData4 %>% filter(total_econ > 2*10^3) %>%
  group_by(EVTYPE, economic_events) %>%
  summarise(median_impact = median(dollars), max_impact = max(dollars)) %>%
  arrange(desc(median_impact))
stormTable2[1:6,]
=======
# Coursera Data Science Specialization Course: Reproducible Resarch
# Course Project 2
# ########################################################################################
# Assignment
# 
# The basic goal of this assignment is to explore the NOAA Storm Database and answer some
# basic questions about severe weather events. You must use the database to answer the 
# questions below and show the code for your entire analysis. Your analysis can consist of
# tables, figures, or other summaries. You may use any R package you want to support your 
# analysis.
# 
# Questions
# 
# Your data analysis must address the following questions:
#   
# 1. Across the United States, which types of events (as indicated in the EVTYPE variable) 
# are most harmful with respect to population health? 
# 2. Across the United States, which types of events have the greatest economic consequences?
# 
# Consider writing your report as if it were to be read by a government or municipal manager 
# who might be responsible for preparing for severe weather events and will need to prioritize
# resources for different types of events. However, there is no need to make any specific 
# recommendations in your report.
#
########################################################################################
# Required Sections
## Title - briefly summarize report
## Synopsis - at most ten sentences describing and summarizing analysis
## Data Processing - words and code
## Results
## No more than 3 figures
## figures need title, axis labels

########################################################################################
# Data Processing
library(tidyverse)

## Data codebook, source url, download, and import

### Codebook: 
"https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf"

### Discussion Forum Notes
"https://www.coursera.org/learn/reproducible-research/discussions/weeks/4/threads/IdtP_JHzEeaePQ71AQUtYw"
 "According to NOAA the data recording start from Jan. 1950. At that time they recorded
  one event type, tornado. They add more events gradually and only from Jan. 1996 they
  start recording all events type. Since our objective is comparing the effects of different
  weather events, do we need to include all years, even it has only single event type?"

### Source re handling PROPDMEXP, CROPDMGEXP
"https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html"

### Data download
#url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
#download.file(url, destfile = "StormData.csv")
stormData <- read.csv("StormData.csv", stringsAsFactors = FALSE)

## Review data set for structure, variable class, etc.
as.tbl(stormData)
glimpse(as.tbl(stormData))
summary(stormData)

## Treat downloaded data as read-only
stormData1 <- stormData

## Change event begin date varaiable class from character to date
stormData1$BGN_DATE <- as.Date(stormData$BGN_DATE, format = "%m/%d/%Y")

## Change event begin date varaiable class from character to date
stormData2 <- stormData1 %>% filter(BGN_DATE >= "1996-01-01")

## Change event type variable from character to factor
stormData2$EVTYPE   <- factor(stormData2$EVTYPE)
stormData2$PROPDMGEXP   <- factor(stormData2$PROPDMGEXP)
stormData2$CROPDMGEXP   <- factor(stormData2$CROPDMGEXP)
#summary(stormData2)

## Rename important event type to conform to Codebook
stormData2$EVTYPE[stormData2$EVTYPE=="HURRICANE/TYPHOON"] <- "HURRICANE"
stormData2$EVTYPE[stormData2$EVTYPE=="TYPHOON"] <- "HURRICANE"
stormData2$EVTYPE[stormData2$EVTYPE=="STORM SURGE"] <- "STORM SURGE/TIDE"
stormData2$EVTYPE[stormData2$EVTYPE=="TSTM WIND"] <- "THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="TSTM WIND/HAIL"] <- "THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="MARINE TSTM WIND"] <- "MARINE THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="WILD/FOREST FIRE"] <- "WILDFIRE"
stormData2$EVTYPE[stormData2$EVTYPE=="FOG"] <- "DENSE FOG"
stormData2$EVTYPE[stormData2$EVTYPE=="URBAN/SML STREAM FLD"] <- "FLOOD"
stormData2$EVTYPE[stormData2$EVTYPE=="WINTER WEATHER/MIX"] <- "WINTER WEATHER"
stormData2$EVTYPE[stormData2$EVTYPE=="HEAVY SURF/HIGH SURF"] <- "HIGH SURF"
stormData2$EVTYPE[stormData2$EVTYPE=="TSTM WIND (G45)"] <- "THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="TSTM WIND (G40)"] <- "THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="STRONG WINDS"] <- "HIGH WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="FREEZING RAIN"] <- "SLEET"
stormData2$EVTYPE[stormData2$EVTYPE=="EXTREME WINDCHILL TEMPERATURES"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="COLD/WIND CHILL"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="EXTREME COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="EXTREME WINDCHILL"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="RECORD COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="UNSEASONABLY COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="Cold"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="UNUSUALLY COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="SNOW"] <- "HEAVY SNOW"
stormData2$EVTYPE[stormData2$EVTYPE=="Snow"] <- "HEAVY SNOW"
stormData2$EVTYPE[stormData2$EVTYPE=="EXCESSIVE SNOW"] <- "HEAVY SNOW"
stormData2$EVTYPE[stormData2$EVTYPE=="WIND"] <- "HIGH WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="Heavy Rain"] <- "HEAVY RAIN"
stormData2$EVTYPE[stormData2$EVTYPE=="RECORD RAINFALL"] <- "HEAVY RAIN"
stormData2$EVTYPE[stormData2$EVTYPE=="RECORD WARMTH"] <- "HEAT"
stormData2$EVTYPE[stormData2$EVTYPE=="UNUSUAL WARMTH"] <- "HEAT"
stormData2$EVTYPE[stormData2$EVTYPE=="RECORD HEAT"] <- "HEAT"
stormData2$EVTYPE[stormData2$EVTYPE=="UNSEASONABLY WARM"] <- "HEAT"
stormData2$EVTYPE[stormData2$EVTYPE=="Winter Weather"] <- "WINTER WEATHER"
stormData2$EVTYPE[stormData2$EVTYPE=="FREEZE"] <- "FROST/FREEZE"
stormData2$EVTYPE   <- factor(stormData2$EVTYPE)
summary(stormData2$EVTYPE)

### STILL WORKING ON RECODING EVTYPE  - COPY ABOVE SECTION TO RMD

## For 50 States Only use stormData[(stormData$STATE %in% state.abb),]

## Mutate economic damage variables to common numeric/dollar basis, incorporating exponent variables
## Rescaled to Millions of Dollars ($000,000)
stormData3 <- stormData2 %>%
  mutate(prop_damage = ifelse(PROPDMGEXP == "", PROPDMG*10^-6, 
    ifelse(PROPDMGEXP == "B", PROPDMG*10^3, 
    ifelse(PROPDMGEXP == "M", PROPDMG*10^0, 
    ifelse(PROPDMGEXP == "K", PROPDMG*10^-3,
    ifelse(PROPDMGEXP ==  0,   PROPDMG*10^-5, PROPDMG)))))) %>%
  mutate(crop_damage = ifelse(CROPDMGEXP == "", CROPDMG*10^-6, 
    ifelse(CROPDMGEXP == "B", CROPDMG*10^3, 
    ifelse(CROPDMGEXP == "M", CROPDMG*10^0, 
    ifelse(CROPDMGEXP == "K", CROPDMG*10^-3,
    ifelse(CROPDMGEXP ==  0,  CROPDMG*10^-5, CROPDMG)))))) 


## Checking & Correcting Entries
# Incorrect exponent - per comments
stormData3 %>% filter(prop_damage == 1.15e+05)
stormData3$prop_damage[stormData3$prop_damage == 1.15e+05] <- 115
# stormData3 %>% filter(crop_damage == 1.51e+03) # Katrina
# stormData3 %>% filter(prop_damage == 3.13e+04) # Katrina
summary(stormData3)


## Create total impact variables; reshape to tidy data
stormData4 <- stormData3  %>%
  mutate(total_econ  = prop_damage + crop_damage) %>%
  mutate(total_human = FATALITIES + INJURIES) %>%
  rename(fatalities = FATALITIES) %>% rename(injuries = INJURIES) %>%
  gather("human_events", "human_count", fatalities:injuries) %>%
  gather("economic_events", "dollars", prop_damage, crop_damage) %>%
  select(EVTYPE, BGN_DATE, human_events, human_count, total_human, economic_events, dollars, total_econ)
head(stormData4)  
  
# Question 1
## Event vs. Population Health: Faceted Boxplots
stormData4 %>% filter(total_human > 201) %>%
  ggplot(aes(x = EVTYPE, y = human_count)) + geom_boxplot() + 
    facet_wrap(~human_events, scales = "free") + scale_y_log10() +
  labs(title = "Population Health Impact", x = "Event Type", y = "Number")

## Event vs. Population Health: Table
stormTable1 <- stormData4 %>% filter(total_human > 201) %>%
  group_by(EVTYPE, human_events) %>%
  summarise(median_impact = median(human_count, na.rm = TRUE), 
            max_impact = max(human_count)) %>%
  arrange(desc(median_impact)) 
stormTable1
  
# Question 2: Event Type vs. Economic Damages
## Event Type vs Property Damage: Top 4-7
stormData4 %>% filter(total_econ > 2*10^3) %>%
  ggplot(aes(x = EVTYPE, y = dollars, col = economic_events)) + geom_boxplot() + 
   scale_y_log10() +
    labs(title = "Economic Impact", x = "Event Type", y = "$ Millions")

stormTable2 <- stormData4 %>% filter(total_econ > 2*10^3) %>%
  group_by(EVTYPE, economic_events) %>%
  summarise(median_impact = median(dollars), max_impact = max(dollars)) %>%
  arrange(desc(median_impact))
stormTable2[1:6,]
>>>>>>> d0ba14015a07f431d7371b06cb198928d1344414
