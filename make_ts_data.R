# Make TS Data
# This code:
#         imports from cloud QA'd csv
#         constructs daily ts to 'regularise' ts
#         compresses ts to monthly observations
#         interpolates (linear) missing values
#
# by Bart Huntley 18/05/2015

rm(list=ls())

library(lubridate)
library(dplyr)
library(zoo)

dir <- "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\cusum"
setwd(dir)

#Read in QA'd data
df <- read.csv("test_data_2015_orig.csv", header = TRUE)
df$date <- dmy(df$date)#create date here to order by
df <- df[order(df$date),]#Very Important - order it!

#Create dummy df of daily timestamps
all <- seq(df[1,1], tail(df[,1], n=1), by = 'days') #daily date sequence for whole period
d <- rep('NA', length(all)) #dummy data column for data frame
alldates <- data.frame(date = all, val = d)#make df of complete date seq

#Create daily ts of site data
df2 <- left_join(alldates, df, by = "date")#join datasets for whole seq
df2 <- df2[,-2]#drop dummy column

#Compress and summarise to mthly observations
df3 <- df2 %>%
        mutate(month = month(date), year = year(date)) %>%
        group_by(year, month) %>%
        summarise_each(funs(mean(., na.rm = TRUE)))
df3[is.na(df3)] <- NA #replaces NaN's
df3[,3] <- seq(ymd('1987-02-01'), ymd('2015-10-01'), by = 'months')#clean date vals to 1st of each mth
df3 <- df3[,c(-1,-2)]#drop year and mth columns

#Interpolate missing values
df4 <- data.frame( df3[,1], na.approx(df3[,2:65]))
