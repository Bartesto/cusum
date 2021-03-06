# This function uses the "changepoint" package to create a ggplot object of a 
# time series of cover values showing where breaks in means occur.
# 
# The function uses the data frame "...mtsd.csv" as the starting point.
# This data frame must be created first.
# 
# By Bart Huntley 25/05/2015

rm(list=ls())

dir="Z:\\DEC\\Vegmachine_SharkBay\\Working\\Bernier_Dorre\\Working\\analysis"
csv="Bernier_Dorre_mtsd.csv"
out=".pdf"
survey="1998-09-01"



chgpt_means <- function(dir, csv, survey, out){
        
        library(lubridate)
        library(ggplot2)
        library(dplyr)
        library(tidyr)
        library(grid)
        library(gridExtra)
        library(changepoint)
        
        ##Generic tasks
        setwd(dir)
        df <- read.csv(csv, header = TRUE)
        df <- df[,-1]
        df[,1] <- as.Date(df[,1])
        
        sname <- names(df)[-1]
        folder <- paste0("ts-chgpt-graphs-", Sys.Date())
        dir.create(folder)
        setwd(paste(dir,folder, sep="\\"))

        for (i in 1:length(sname)){
                
                df2.i <- df[, c(1, 1+i)]
                df2.i$label <- factor(rep("ts", by=length(df2.i[,1])))
                
                #Helper function
                indexer <- function(){
                        ev <- vector(mode="numeric", length=length(cpts))
                        for(j in 1:length(cpts)){
                                ev[j] <- as.numeric(df2.i[cpts[j],1])
                        }
                        ev
                }
                #Changepoint
                site.ts <- ts(df2.i[,2], frequency=12, 
                              start=c(as.numeric(year(df2.i[1,1])),
                                      as.numeric(month(df2.i[1,1]))),
                              end=c(as.numeric(year(tail(df2.i[,1], n=1))),
                                    as.numeric(month(tail(df2.i[,1], n=1)))))
                mvalue <- cpt.mean(site.ts, method="BinSeg")
                
                cpts <- mvalue@cpts
                
                #raw dates of chgpts - mix of star and end dates for periods
                date <- c(df[1,1], as.Date(indexer()))
                
                cptdf <- data.frame(start=date[1:length(date)-1],
                                    end=date[-1], y=mvalue@param.est$mean, label=factor("mean"))
                #df for vertical line handling
                vertdf <- data.frame(x=as.Date(survey), y=c(-Inf, Inf),
                                     Survey=factor(year(as.Date(survey))))
                
                p2 <- ggplot()+
                        geom_point(data=df2.i, aes_string(x="date", y=sname[i], colour="label"))+
                        geom_line(data=df2.i, aes_string(x="date", y=sname[i], colour="label"))+
                        scale_colour_manual(values=c("black"),
                                            name=sname[i],
                                            breaks=as.character(df2.i$label),
                                            labels=as.character(df2.i$label))+
                        geom_segment(data=cptdf, aes(x=start, y=y, xend=end, yend=y), colour="red")+
                        geom_line(aes(x,y, linetype=Survey), colour='blue', size = 0.5, vertdf)+
                        coord_cartesian(ylim = c(-10, 80))+
                        theme_bw()+
                        xlab("")+
                        ylab("Vegetation Cover %")+
                        theme(legend.justification=c(0,1), 
                              legend.position=c(0,1),
                              axis.title.y= element_text(size=15),
                              axis.text.y = element_text(angle=90, size=15),
                              axis.text.x = element_text(size=15),
                              legend.title = element_text(size = 15),
                              legend.text = element_text(size = 15)) 
                
                
                sname.i<-sname[i]
                filename<-paste0(sname.i, "-chgpt-plot",out)

                ggsave(file=filename, p2, width = 22.5, height = 13.5, units = "cm")
        }
}

chgpt_means(dir, csv, survey, out)
