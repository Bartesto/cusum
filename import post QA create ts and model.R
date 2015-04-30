## Time series & Modelling
# 1. import .csv from cloud QA
# 2. interpolate NA values
# 3. create regular ts object
# 4. model seasonality
# 5. plot interpolated ts and seasonal model

rm(list = ls())

library(lubridate)
library(dplyr)
library(tidyr)
library(zoo)

##Direct import of QA'd csv can graph (deleted values import as NA's)
df <- read.csv("test_data_2015_orig.csv", header = TRUE)
df$date <- dmy(df$date)#create date here to order by
df <- df[order(df$date),]#Very Important - order it!


##Get data into regular object (mthly) with data averaged where needed to use ts
all <- seq(ymd('1987-02-09'), ymd('2015-10-01'), by = 'days') #daily date sequence for whole period
d <- rep('NA', 10462) #dummy column for data frame
alldates <- data.frame(date = all, val = d)#make df of complete date seq

df2 <- left_join(alldates, df, by = "date")#join datasets for whole seq
df2 <- df2[,-2]#drop dummy column

#create summary by month (still has missing months coded NaN)
df3 <- df2 %>%
        mutate(month = month(date), year = year(date)) %>%
        group_by(year, month) %>%
        summarise_each(funs(mean(., na.rm = TRUE)))
df3[is.na(df3)] <- NA #replaces NaN's

#work with one site at time
d1 <- df3$dhi_05

#Interpolate missing values (linear interpolation)
int.d1 <- na.approx(d1)

#Create ts object matching start and end to period
ts.d1 <- ts(int.d1, start=c(1987, 2), end=c(2015, 10), frequency=12)

#Create ts model
fit.d1 <- stl(ts.d1, s.window="period")

#Extract seasonal component and create model from mean of actual values
seas.d1 <- fit.d1$time.series[,"seasonal"]
seasmod.d1 <- mean(int.d1)+seas.d1


#Plot results - overlays interploated time series with seasonal model
plot(ts.d1, ylim=c(0,80))
par(new=T)
plot(seasmod.d1, col = 'red', ylim=c(0,80))


#Plots the classic view, actual, seasonal, trend, noise
plot(fit.d1)


