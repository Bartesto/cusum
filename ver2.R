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




##Google causalimpacts
install.packages("devtools")
library(devtools)
devtools::install_github("google/CausalImpact")
library(CausalImpact)
matplot(ts.i, type = "l")

pre.period <- as.Date(c("1987-02-18", "2008-01-01"))
post.period <- as.Date(c("2008-01-02", "2015-10-01"))
impact <- CausalImpact()