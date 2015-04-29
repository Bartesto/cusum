rm(list = ls(all = T))#Clears workspace if required

##work on longer ts for modelling

library(lubridate)
library(dplyr)
library(tidyr)
library(zoo)



df <- read.csv("test_data_2015_orig.csv", header = TRUE)
df$date <- dmy(df$date)#use lubridate
df <- df[order(df$date),]#make sure df is ordered by date

##Get data in regular ts shape (mthly) with data averaged where needed
all <- seq(ymd('1987-02-09'), ymd('2015-10-01'), by = 'days') #daily date sequence for whole period
d <- rep('NA', 10462) #dummy column for data frame
alldates <- data.frame(date = all, val = d)#make df of complete date seq


df2 <- left_join(alldates, df, by = "date")#join datasets for whole seq
df2 <- df2[,-2]#drop dummy column

df3 <- df2 %>%
        mutate(month = month(date), year = year(date)) %>%
        group_by(year, month) %>%
        summarise_each(funs(mean(., na.rm = TRUE)))
df3[is.na(df3)] <- NA #replaces NaN's
