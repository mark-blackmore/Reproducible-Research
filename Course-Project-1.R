# Coursera Data Science Specialization Course: Reproducible Resarch
# Course Project 1
# ########################################################################################
# 
# Introduction
# 
# It is now possible to collect a large amount of data about personal movement using
# activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type
# of devices are part of the "quantified self" movement - a group of enthusiasts who take
# measurements about themselves regularly to improve their health, to find patterns in
# their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data
# are hard to obtain and there is a lack of statistical methods and software for processing
# and interpreting the data.
# 
# This assignment makes use of data from a personal activity monitoring device. This device
# collects data at 5 minute intervals through out the day. The data consists of two months
# of data from an anonymous individual collected during the months of October and November,
# 2012 and include the number of steps taken in 5 minute intervals each day.
# 
# The data for this assignment can be downloaded from the course web site:
# 
# The variables included in this dataset are:
# 
# * steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
# * date: The date on which the measurement was taken in YYYY-MM-DD format
# * interval: Identifier for the 5-minute interval in which measurement was taken
# 
# The dataset is stored in a comma-separated-value (CSV) file and there are a total of
# 17,568 observations in this dataset.
#
#######################################################################################
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(VIM)
library(caret)
library(mice)
set.seed(1010)

# 1. Code for reading in the dataset and/or processing the data
activity <- read_csv("activity.csv")
activity
str(activity)
summary(activity)

# 2. Histogram of the total number of steps taken each day
activity %>% group_by(date) %>% summarise(stepsPerDay = sum(steps)) %>% 
  ggplot(aes(x = stepsPerDay)) + geom_histogram(bins = 15) + 
  ggtitle("Histogram: Total Number of Steps Per Day") + xlab("Steps Per Day") + 
  ylab("Frequency")

# 3. Mean and median number of steps taken each day
## Mean steps, missing values removed
activity %>% group_by(date) %>% 
  summarise(meanStepsPerDay = mean(steps, na.rm = TRUE)) %>% 
  summarise(meanSteps = mean(meanStepsPerDay, na.rm = TRUE))

## Median steps, missing values removed
activity %>% group_by(date) %>% 
  summarise(medianStepsPerDay = median(steps, na.rm = TRUE)) %>%
  summarise(medianSteps = median(medianStepsPerDay, na.rm = TRUE))

# 4.Time series plot of the average number of steps taken
activity %>% group_by(date) %>% summarise(meanSteps = mean(steps, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = meanSteps)) + geom_line() + 
  ggtitle("Mean Steps by Date") + xlab("Date") + ylab("Mean Steps")

# # 4a. Time series plot of total steps per day
# activity %>% group_by(date) %>% summarise(totalSteps = sum(steps)) %>%
#   ggplot(aes(x = date, y = totalSteps)) + geom_line() + 
#   ggtitle("Total Steps by Date") + xlab("Date") + ylab("Mean Steps")
# 
# # 4b. Time series plot of total steps per day, missing values removed
# activity %>% group_by(date) %>% summarise(totalSteps = sum(steps, na.rm = TRUE)) %>%
#   ggplot(aes(x = date, y = totalSteps)) + geom_line() + 
#   ggtitle("Total Steps by Date") + xlab("Date") + ylab("Mean Steps")

# 5. The 5-minute interval that, on average, contains the maximum number of steps
activity %>% group_by(interval) %>% 
  summarize(meanByInterval = mean(steps, na.rm = TRUE)) %>%
  filter(meanByInterval == max(meanByInterval))

# 5a. The 5-minute interval that, on average, contains the minimum number of steps
activity %>% group_by(interval) %>% 
  summarize(meanByInterval = mean(steps, na.rm = TRUE)) %>%
  filter(meanByInterval == min(meanByInterval))
  
# 6. Code to describe and show a strategy for imputing missing data
## Missing values by varaiable
md.pattern(activity)
sum(is.na(activity))

## Missing Values as percent of total, percent each columns, percent each rows
sum(is.na(activity))/(dim(activity)[1]*dim(activity)[2]) * 100 
pMiss <- function(x) { sum(is.na(x)) / length(x) * 100}
apply(activity, 2, pMiss)

## Missing values visualization
aggr(activity, numbers = TRUE)
marginplot(activity[c(3,1)])

# ## Median imputation of missing values
# preProcessValues <- preProcess(activity, method = "medianImpute")
# activityProcessed <- predict(preProcessValues, activity)
# aggr(activityProcessed, numbers = TRUE)
# marginplot(activityProcessed[c(3,1)])

## Complete cases only
activityNoMissing <- activity[complete.cases(activity),]

# 7. Histogram of the total number of steps taken each day after missing values are imputed
# activityProcessed %>% group_by(date) %>% summarise(stepsPerDay = sum(steps)) %>% 
#   ggplot(aes(x = stepsPerDay)) + geom_histogram(bins = 15) + 
#   ggtitle("Histogram: Total Number of Steps Per Day") + xlab("Steps Per Day") + 
#   ylab("Frequency")

# 7a.Histogram of the total number of steps taken each day, complete cases only
activityNoMissing %>% group_by(date) %>% summarise(stepsPerDay = sum(steps)) %>% 
  ggplot(aes(x = stepsPerDay)) + geom_histogram(bins = 15) + 
  ggtitle("Histogram: Total Number of Steps Per Day") + xlab("Steps Per Day") + 
  ylab("Frequency")

# 7a. Time series plot of average number of steps taken after missing values are imputed
# activityProcessed %>% group_by(date) %>% summarise(meanSteps = mean(steps, na.rm = TRUE)) %>%
#   ggplot(aes(x = date, y = meanSteps)) + geom_line() +
#   ggtitle("Mean Steps by Date") + xlab("Date") + ylab("Mean Steps")

activityNoMissing %>% group_by(date) %>% summarise(meanSteps = mean(steps, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = meanSteps)) + geom_line() + 
  ggtitle("Mean Steps by Date") + xlab("Date") + ylab("Mean Steps")

#7b. Mean and Median post missing value imputation, comparison
# ## Mean steps
# activityProcessed %>% group_by(date) %>% 
#   summarise(meanStepsPerDay = mean(steps)) %>% 
#   summarise(meanSteps = mean(meanStepsPerDay))
# 
# ## Median steps
# activityProcessed %>% group_by(date) %>% 
#   summarise(medianStepsPerDay = median(steps)) %>%
#   summarise(medianSteps = median(medianStepsPerDay))

## Mean decreased, median stayed the dame
## Using complete cases, both mean and median were unchanged

# # 8. Panel plot comparing the average number of steps taken per 5-minute interval 
# # across weekdays and weekends
# t <- activityProcessed %>% mutate(dayOfWeek = weekdays(date)) %>%
#   mutate(Weekend = ifelse(dayOfWeek == "Saturday" | dayOfWeek == "Sunday", "Weekend", "Weekday"))
# 
# ## By Weekday vs. Weekend 
# t %>% 
#   group_by(Weekend, interval) %>% mutate(meanStepsInterval = mean(steps)) %>%
#   ggplot(aes(x = interval, y = meanStepsInterval)) + geom_line() +
#   facet_wrap(~Weekend) +ggtitle("Mean Steps by Interval: Weekday vs. Weekend") + 
#   xlab("Interval") + ylab("Mean Steps")
# 
# # ## By Days of the Week
# t %>%
#   group_by(dayOfWeek, interval) %>% mutate(meanStepsInterval = mean(steps)) %>%
#   ggplot(aes(x = interval, y = meanStepsInterval)) + geom_line() +
#   facet_wrap(~dayOfWeek) +ggtitle("Mean Steps by Interval: By Day") +
#   xlab("Interval") + ylab("Mean Steps")

###  Complete cases
# 8. Panel plot comparing the average number of steps taken per 5-minute interval 
# across weekdays and weekends
t <- activityNoMissing %>% mutate(dayOfWeek = weekdays(date)) %>%
  mutate(Weekend = ifelse(dayOfWeek == "Saturday" | dayOfWeek == "Sunday", "Weekend", "Weekday"))

## By Weekday vs. Weekend 
t %>% 
  group_by(Weekend, interval) %>% mutate(meanStepsInterval = mean(steps)) %>%
  ggplot(aes(x = interval, y = meanStepsInterval)) + geom_line() +
  facet_wrap(~Weekend) +ggtitle("Mean Steps by Interval: Weekday vs. Weekend") + 
  xlab("Interval") + ylab("Mean Steps")

# ## By Days of the Week
t %>%
  group_by(dayOfWeek, interval) %>% mutate(meanStepsInterval = mean(steps)) %>%
  ggplot(aes(x = interval, y = meanStepsInterval)) + geom_line() +
  facet_wrap(~dayOfWeek) +ggtitle("Mean Steps by Interval: By Day") +
  xlab("Interval") + ylab("Mean Steps")


