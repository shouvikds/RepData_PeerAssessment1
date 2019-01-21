---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
##Required Libraries
'''{r}
library(ggplot2)
library(knitr)
library(dplyr)

'''

## Loading and preprocessing the data
'''{r}
setwd("C:/Users/shodas/Documents/Data Science Coursera/RepData_PeerAssessment1")
activityData <- read.csv(unz("activity.zip", "activity.csv"), header = TRUE,
                         sep = ",") 
summary(activityData)
str(activityData)
'''

## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
