rm(list = ls())

library(lubridate)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
#### STAGE ONE ####

start_date <- "1987-02-09"
base_end <- "2008-01-01"

##Direct import of QA'd csv can graph (deleted values import as NA's)
df <- read.csv("test_data_2015_orig.csv", header = TRUE)
df$date <- dmy(df$date)#create date here to order by
df <- df[order(df$date),]#Very Important - order it!
# s.date <- df$date[1]
# e.date <- tail(df$date, n=1)
# time.points <- seq.Date(s.date, e.date, by = 1)

##Get data into regular object (mthly) with data averaged where needed to use ts
all <- seq(df[1,1], tail(df[,1], n=1), by = 'days') #daily date sequence for whole period
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



#df2 is daily
#df3 is mthly

#base periods for modelling
base.df2 <- df2 %>%
        filter(date < base_end)

base.df3 <- df3 %>%
        filter(date < base_end)

#model for daily at site 17 NOT WORKING
mod.d <- na.approx(base.df2[,18])
mod.d.ts <- ts(mod.d, start=c(min(base.df2[,1]), as.numeric(head(df3[,2], n=1))), 
   end=c(max(df3[,1]), as.numeric(tail(df3[,2], n=1))), frequency=12)


#set up daily data for site 17
dy.df <- df2[,c(1,18)]

site <- na.approx(dy.df[,2])
date <- as.Date(as.character(dy.df[,1]))
data <- zoo(site, date)

#model for mthly
mod.m <- na.approx(base.df3[,20])
mod.m.ts <- ts(mod.m, start=c(min(base.df3[,1]), as.numeric(head(base.df3[,2], n=1))), 
               end=c(year(base_end), month(base_end)), frequency=12)
fit.m <- stl(mod.m.ts, s.window="period")
seas.m <- fit.m$time.series[,"seasonal"]
seasmod.m <- mean(mod.m.ts) + as.numeric(fit.m$time.series[,"seasonal"])
seasmod.m <- seasmod.m[1:12]




#set up mthly data for site 17
mths <- seq.Date(as.Date(start_date), as.Date("2015-10-10"), by = "month")
#msite <- na.approx(df3[,4])
msite17 <- na.approx(df3[,20])
#datam1 <- zoo(msite, mths)
datam17 <- zoo(cbind(msite17, seasmod.m), mths)

matplot(datam17)



##Google causalimpacts
install.packages("devtools")
library(devtools)
devtools::install_github("google/CausalImpact")
library(CausalImpact)
matplot(dy.df, type = "l")

pre.period <- as.Date(c("1987-02-18", "2008-01-01"))
post.period <- as.Date(c("2008-01-02", "2015-10-01"))



#daily obs for site 17
impact17d <- CausalImpact(data, pre.period, post.period, 
                          model.args = list(nseasons = 28, season.duration = 31))
d17d <- plot(impact17d, c("original", "cumulative")) + ggtitle("site17 daily")
plot(d17d)
summary(impact17d)
summary(impact17d, "report")





impact17 <- CausalImpact(datam17, pre.period, post.period, model.args = list(nseasons = 28, season.duration = 12))
d17 <- plot(impact17) +ggtitle("site17 monthly")
plot(d17)
summary(impact17)
summary(impact17, "report")
