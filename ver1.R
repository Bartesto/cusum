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
#create base period time series to model from
base_end=dmy("01/01/2008")#######VARIABLE INPUT
base.df <- df3 %>%
        filter(date < base_end)

#### STAGE 3 ####

#df3 is complete data frame with all sites
#This stage takes the complete data frame and breaks the remaining calcs by site

i=2
#Time Series per site for all period
site.i <- na.approx(df3[,3+i])#interpolated for site all
ts.i <- ts(site.i, start=c(min(df3[,1]), as.numeric(head(df3[,2], n=1))), 
                end=c(max(df3[,1]), as.numeric(tail(df3[,2], n=1))), frequency=12)

#Time series per site for base period
site.base.i <- na.approx(base.df[,3+i])#interpolated for site base
ts.base.i <- ts(site.base.i, start=c(min(base.df[,1]), 
                                     as.numeric(head(base.df[,2], n=1))), 
                end=c(year(base_end), month(base_end)), frequency=12)


#Find seasonal component of base period as "model'
fit.i <- stl(ts.base.i, s.window="period")
seas.i <- fit.i$time.series[,"seasonal"]
seasmod.i <- mean(ts.base.i) + as.numeric(fit.i$time.series[,"seasonal"])
seasmod.i <- seasmod.i[1:12] #reduce to 12 monthly repeating values

#create temp df and extrapolate model for length of all time series
#putting in temp df allowed me to recycle model to complete length
tmp.df.i <- cbind(site.i, seasmod.i)
ext.mod.i <- ts(tmp.df.i[,2], start=c(min(df3[,1]), as.numeric(head(df3[,2], n=1))), 
                end=c(max(df3[,1]), as.numeric(tail(df3[,2], n=1))), frequency=12)




plot(ts.i, ylim=c(0,80))
par(new=T)
plot(ext.mod.i, col = 'red', ylim=c(0,80))

##Google causalimpacts
install.packages("devtools")
library(devtools)
devtools::install_github("google/CausalImpact")
library(CausalImpact)
matplot(ts.i, type = "l")

pre.period <- as.Date(c("1987-02-18", "2008-01-01"))
post.period <- as.Date(c("2008-01-02", "2015-10-01"))
impact <- CausalImpact()

