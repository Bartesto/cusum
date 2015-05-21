# Make TS Data
# This function:
#         imports from cloud QA'd csv
#         constructs daily ts to 'regularise' ts
#         compresses ts to monthly observations
#         interpolates (linear) missing values
#         writes new data to csv
#
# by Bart Huntley 18/05/2015

rm(list=ls())



dir="Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\cusum"
csv="test_data_2015_orig.csv"
project="dhi"


mtsd <- function(dir, csv, project) {
        library(lubridate)
        library(dplyr)
        library(zoo)
        setwd(dir)
        df <- read.csv(csv, header = TRUE)
        df$date <- dmy(df$date)#create date here to order by
        df <- df[order(df$date),]#Very Important - order it!
        all <- seq(df[1,1], tail(df[,1], n=1), by = 'days') #daily date sequence for whole period
        d <- rep('NA', length(all)) #dummy data column for data frame
        alldates <- data.frame(date = all, val = d)#make df of complete date seq
        df2 <- left_join(alldates, df, by = "date")#join datasets for whole seq
        df2 <- df2[,-2]#drop dummy column
        df3 <- df2 %>%
                mutate(month = month(date), year = year(date)) %>%
                group_by(year, month) %>%
                summarise_each(funs(mean(., na.rm = TRUE)))
        df3[is.na(df3)] <- NA #replaces NaN's
        emd <- ymd(paste0(as.character(tail(df3[,1], n=1)), 
                          "-", as.character(tail(df3[,2], n=1)), 
                          "-", as.character(day(df[1,1]))))#endmonthday 
        df3[,3] <- seq(df[1,1], emd, by = 'months')#clean date vals to reg day of each mth
        df3 <- df3[,c(-1,-2)]#drop year and mth columns
        df4 <- data.frame( df3[,1], na.approx(df3[,-1], rule=2))
        write.csv(df4, file = paste0(project, "_mtsd.csv"))
        
}