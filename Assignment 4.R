#'Crystal Valdez
#'Assignment 4 2011 


setwd("~/Downloads/")
#get the working directory
getwd()
library(lubridate)
library(dplyr)

ufo <- read.csv("ufo_subset.csv")

finalsolution <- ufo %>% 
  #clean the rows that do no have country or shape information 
  filter(!is.na(country)) %>%
  filter(!is.na(shape)) %>%
  #convert datetime and date posted to same POSIXlt format
  mutate(datetime = as.POSIXlt(datetime, tz = "UTC", "%Y-%m-%d")) %>%
  mutate(date.posted = as.POSIXlt(date.posted, tz = "UTC", "%Y-%m-%d")) %>%
  #filter out comments that mention Hoax or NUFORC that may be hoax
  filter(!grepl("\\b[HOAX]", comments, ignore.case = T)) %>%
  filter(!grepl("\\b[NUFORC]", comments, ignore.case = T))%>%
  #add column to populate time difference in days between date of sighting and report
  mutate(report_delay = as.Date(date.posted)- as.Date(datetime)) %>%
  #filter out rows where report delay is negative
  filter(report_delay > 0) %>%
  #check the data quality - changing all values to decimal and renaming column
  mutate(duration..seconds. = round(duration..seconds., 2)) %>%
  rename(duration_in_secs = duration..seconds.)

#continue data quality check - finding lower and upper bounds to exclude outliers in the data
Q1 <- quantile(finalsolution$duration_in_secs, prob = .25)
Q3 <- quantile(finalsolution$duration_in_secs, prob = .75)

lower <- Q1 - (1.5*(IQR(finalsolution$duration_in_secs)))
upper <- Q3 + (1.5*(IQR(finalsolution$duration_in_secs)))

withoutoutliers <- finalsolution %>%
  filter(duration_in_secs > lower & duration_in_secs < upper)

#create table with average report delay per country 
report_table <- finalsolution %>%
  group_by(country)%>%
  summarise(average_report_delay = mean(report_delay), na.rm = T)

#plot histogram of log duration in seconds  
hist(log(ufo$duration..seconds.), main = "Histogram of Duration in Seconds", xlab = "log duration in seconds", ylab = "frequency of observations") 
  
  
  



