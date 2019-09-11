library(tidyverse)
library(here)
library(lubridate)
library(XML)

#load apple health export.xml file
xml <- xmlParse(here::here("201909", "export.xml"))

#transform xml file to data frame - select the Record rows from the xml file
df <- XML:::xmlAttrsToDataFrame(xml["//Record"])

#make value variable numeric
df$value <- as.numeric(as.character(df$value))

#make endDate in a date time variable POSIXct using lubridate with eastern time zone
df$endDate <-ymd_hms(df$endDate,tz = "Europe/Stockholm")

##add in year month date dayofweek hour columns
df$month<-format(df$endDate,"%m")
df$year<-format(df$endDate,"%Y")
df$date<-format(df$endDate,"%Y-%m-%d")
df$dayofweek <-wday(df$endDate, label=TRUE, abbr=FALSE)
df$hour <-format(df$endDate,"%H")

# write_csv(df, here::here("201909", "applehealthdata.csv"))
