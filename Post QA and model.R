####This picks up after QA of cloud has produced a csv of clean data.


rm(list = ls(all = T))#Clears workspace if required

##proving direct import of QA'd csv can graph (deleted values import as NA's)
df <- read.csv("10970_i35_test.csv", header = TRUE)
df <- df[,-1]
df$date <- as.Date(df$date, '%d/%m/%Y')

plot(Bau_002 ~ date, df)
lines(Bau_002 ~ date, df, col = "red")


rm(list = ls(all = T))#Clears workspace if required

##work on longer ts for modelling

library(lubridate)
library(dplyr)
library(tidyr)
library(zoo)



df <- read.csv("test_data_2015_orig.csv", header = TRUE)
df$date <- dmy(df$date)
df <- df[order(df$date),]

##plot to see
plot(dhi_02 ~ date, df)
lines(dhi_02 ~ date, df, col = "red")

##two functions to swap between decimal dates
date_to_decimal <- function(date){
        
        if(any(!inherits(date, c("POSIXt", "POSIXct", "POSIXlt", "Date"))))
                stop("date(s) not in POSIXt or Date format")
        
        date <- force_tz(as.POSIXlt(date), tz = "UTC")
        Y <- year(date)
        ## parsing is much faster than updating
        start <- parse_date_time2(paste(Y, "1", "1"), "Ymd")
        end <- parse_date_time2(paste(Y + 1L, "1", "1"), "Ymd")
        sofar <- as.numeric(difftime(date, start, units = "secs"))
        total <- as.numeric(difftime(end, start, units = "secs"))
        Y + sofar/total
}
decimal_to_date <- function(decimal, tz = NULL) {
        Y <- trunc(decimal)
        ## parsing is much faster than updating
        start <- parse_date_time2(paste(Y, "01", "01"), "Ymd")
        end <- parse_date_time2(paste(Y + 1L, "01", "01"), "Ymd")
        seconds <- as.numeric(difftime(end, start, units = "secs"))
        frac <- decimal - Y
        start <- start + seconds * frac
        if (!is.null(tz))
                force_tz(start, tz)
        else
                start
}

## One attempt not working /366 not right for this
# Time <- date_to_decimal(df[,1])
# value <- df[,10]
# 
# xc<-cos(2*pi*Time/366)#time period is wrong
# xs<-sin(2*pi*Time/366)
# fit.lm <- lm(value~xc+xs)
# access the fitted series (for plotting)
# fit <- fitted(fit.lm)

# find predictions for original time series
# pred <- predict(fit.lm, newdata=data.frame(Time=Time))    
# plot(value ~ Time)
# lines(fit, col="red")
# lines(Time, pred, col="blue")



## Another attempt fails due to ts not regular
# ssp <- spectrum(value)  
# per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
# reslm <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t))
# summary(reslm)
# 
# rg <- diff(range(y))
# plot(y~t,ylim=c(min(y)-0.1*rg,max(y)+0.1*rg))
# lines(fitted(reslm)~t,col=4,lty=2)   # dashed blue line is sin fit

##Regularise the ts
date <- df$date
value <- df[,2]
plot(value ~ date)
lines(value ~ date)

a <-     df %>%
                select(date, dhi_02) %>%
                mutate(month = month(date), year = year(date)) %>%
                group_by(year, month) %>%
                summarise(val = mean(dhi_02))

big <- seq(ymd('1987-02-09'), ymd('2015-10-01'), by = 'days')
val <- rep(0, 10462)
big <- data.frame(date = big, val = val)

testdf <- df %>%
        select(date, dhi_02)

big <- zoo(big)
testdf <- zoo(testdf)
all <- merge(big,testdf)
test2 <- na.locf(merge(testdf,big))

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
plot(df5[[3]])
##plots to compare the series (remember one has been averaged!)
plot(dhi_02 ~ date, data = df3)
par(new=T)
plot(dhi_02 ~ date, data = df, col = 'red')
lines(dhi_02 ~ date, data = df3)
lines(dhi_02 ~ date, data = df, col = 'red')


df4 <- zoo(df3)
df5 <- ts(df3)
test1 <- df4[,c("date", "dhi_01")]
test1[,2] <- na.fill(test1[,2], "extend")

autoplot(df4)
