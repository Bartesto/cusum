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

#Set up model and index for base period
base_end = "01/01/2008"
ts.base <- window(ts.d1, start=c(1987, 2), end=c(2008, 1))
fit.base <- stl(ts.base, s.window="period")
seas.base <- fit.base$time.series[,"seasonal"]
seasmod.base <- mean(int.d1)+seas.d1



action<-my(base_end) #base action date
base.index<- df3[,3]<action

#old code
i= 3 #4th is site 01

ts.i<-ts.d1
mo.i<-seasmod.d1
diff.i<-ts.i-mo.i
#diff.i[is.na(diff.i)]<-0 #converts NA's to 0
cu.i<-cumsum(diff.i)
cu.base<-cu.i[base.index] #cusum for base only for sd calcs
sName.i<-sname[i]
FileName<-paste(sName.i, "cusum",".jpg",sep=" ")
jpeg(file=FileName, width=10*dpi, height=7*dpi, res=dpi)
par(mfrow=c(2,1)) #sets up 2 rows of graphs in 1 column
par(mar=c(2, 4, 1.5, .5)) #manipulating margins to control blank space
plot(mo.i ~ date, data, col="red", ylab= NA, xlab=NA, 
     yaxt="n", xaxt="n",ylim=c(0,80))
lines(mo.i ~ date, data, col="red")
abline(v=as.Date("2008-01-01"), lty=4, lwd=2) #management action date
legend("topleft", legend=c("Landsat Time Series", "1987-2008 Baseline",
                           "2008"), col=c("black","red", "black"), lty=c(1, 1, 4), lwd=2, cex=0.8)
par(new=T) #allows call to plot over an existing plot
plot(ts.i ~ date, data, ylab= "Vegetation Cover %",
     ylim=c(0,80), main=sname[i], xlab=NA, xaxt="n")
lines(ts.i ~ date, data)
plot(cu.i ~ data$date, ylab= "Cumulative Sum", xlab= "Year",
     ylim=c(-1000,1000))
lines(cu.i ~ data$date)
abline(h=stdv*sd(cu.base, na.rm=TRUE), lty=2, lwd=2, col="blue")
abline(h=-stdv*sd(cu.base, na.rm=TRUE), lty=2, lwd=2, col="blue")
abline(0,0)
abline(v=action, lty=4, lwd=2) #management action date
abline(v= as.Date(new_start, '%d/%m/%Y'), lty=4, lwd=2, col = "red") #new from
legend("topleft", legend=c("Cumulative sum of difference to baseline model", "5 standard deviations",
                           "2008", "New data"), col=c("black", "blue", "black", "red"),
       lty=c(1,2,4,4), lwd=2, cex=0.8)
               

