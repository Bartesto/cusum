# Various functions for analysing TS data
# All functions here start with csv export from mtsd function
# 
# Functions:
#         1. Original CUSUM work with seasonal model calculation
#         2. Causal Impacts - Google approach to original methods 
#         using Bayesian statistical approach
#         3. Breakout Detection - Twitter approach to detecting
#         brakouts in TS using non-parametric algorithm (E-Divisive
#         with Medians)
#         4. Changepoint detection - changepoint package detecting 
#         changepoints via means and variances
#
# by Bart Huntley 18/05/2015

rm(list=ls())

#inputs for function
dir="Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\cusum"
csv="dhi_site_TS_data.csv"
base_end <- "2008-01-01"
i=1


#libraries
library(lubridate)
library(ggplot2)




#read data
setwd(dir)
df <- read.csv(csv, header = TRUE)
df <- df[,-1]
df[,1] <- as.Date(df[,1])

#make base df and model
base.df <- df %>%
        filter(date < base_end)

b.i <- base.df[,1+i]
b.ts.i <- ts(b.i, start=c(year(base.df[1,1]), month(base.df[1,1])),
                  end=c(year(tail(base.df[,1], n=1)), month(tail(base.df[,1], n=1))),
                  frequency=12)
fit.b.i <- stl(b.ts.i, s.window="period")                          
seasmod.b.i <- mean(b.ts.i) + as.numeric(fit.b.i$time.series[,"seasonal"])
seasmod.b.i <- seasmod.b.i[1:12]


#make df of site and its model
df2 <- df[, c(1, 1+i)]
df2$model <- rep(seasmod.b.i, length.out = length(df[,1]))

p.i <- ggplot()+
        geom_line(data=df2, aes(x=date, y=df2[,2], colour="blue"))+
        geom_line(data=df2, aes(x=date, y=df2[,3], colour="red"))+
        geom_vline(xintercept=as.numeric(as.Date(base_end)))+
        xlab("date")+
        ylab("cover")
p.i             
             
             
             
             
             