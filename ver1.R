rm(list = ls())

library(lubridate)
library(dplyr)
library(tidyr)
library(zoo)
#### STAGE ONE ####

##Direct import of QA'd csv can graph (deleted values import as NA's)
df <- read.csv("test_data_2015_orig.csv", header = TRUE)
df$date <- dmy(df$date)#create date here to order by
df <- df[order(df$date),]#Very Important - order it!


##Get data into regular object (mthly) with data averaged where needed to use ts
all <- seq(ymd('1987-02-09'), ymd('2015-10-01'), by = 'days') #daily date sequence for whole period
d <- rep('NA', length(all)) #dummy column for data frame
alldates <- data.frame(date = all, val = d)#make df of complete date seq

df2 <- left_join(alldates, df, by = "date")#join datasets for whole seq
df2 <- df2[,-2]#drop dummy column

#create summary by month (still has missing months coded NaN)
df3 <- df2 %>%
        mutate(month = month(date), year = year(date)) %>%
        group_by(year, month) %>%
        summarise_each(funs(mean(., na.rm = TRUE)))
df3[is.na(df3)] <- NA #replaces NaN's

#### STAGE TWO ####

base_end=dmy("01/01/2008")
base.df <- df3 %>%
        filter(date < base_end)

#### STAGE 3 ####

#per site
df3.i <- na.approx(df3[,4])#interpolated site 1
base.df.i <- na.approx(base.df[,4])
ts.d1 <- ts(base.df.i, start=c(min(df3[,1]), as.numeric(head(df3[,2], n=1))), 
                end=c(max(df3[,1]), as.numeric(tail(df3[,2], n=1))), frequency=12)




