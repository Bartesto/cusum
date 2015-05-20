# Various functions for analysing TS data
# All functions here start with mtsd.csv export from mtsd function
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


##Stage 1
oca <- function(dir, csv, base_end, stdev=3, out){
        library(lubridate)
        library(ggplot2)
        library(dplyr)
        library(tidyr)
        library(grid)
        library(gridExtra)
        
        #generic tasks
        setwd(dir)
        df <- read.csv(csv, header = TRUE)
        df <- df[,-1]
        df[,1] <- as.Date(df[,1])
        #make base df
        base.df <- df %>%
                filter(date < base_end)
        sname <- names(df)[-1]
        folder <- paste0("Orig-Cusum-graphs-", Sys.Date())
        dir.create(folder)
        setwd(paste(dir,folder, sep="\\"))
        
        for (i in 1:length(sname)){
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
                df2$cumsum <- cumsum(df2[,2]-df2[,3])
                df2$label <- factor(rep("cumsum", by=length(df2[,1])))
                df3 <- gather(df2, "Series", "Value", 2:3)
                vertdf <-data.frame(x=as.Date(base_end), y=c(-Inf, Inf),
                                    Mngt=factor(year(as.Date(base_end))))
                cu.i<-cumsum(df2[,2]-df2[,3])
                cu.base<-cu.i[1:length(b.i)]#shorten for sd calcs
                hordf <- data.frame(pos=stdev*sd(cu.base), neg=-stdev*sd(cu.base),
                                    x=c(min(df2[,1]), max(df2[,1])), Limit=factor("Limit"))
                
                
                #Model and ts plot
                p1 <- ggplot()+
                        geom_point(data=df3, aes(x=date, y=Value, colour=Series))+
                        geom_line(data=df3, aes(x=date, y=Value, colour=Series))+
                        scale_colour_manual(values=c("black", "red"),
                                            name="Series",
                                            breaks=as.character(df3$Series),
                                            labels=as.character(df3$Series))+
                        coord_cartesian(ylim = c(0, 80))+
                        geom_line(aes(x,y, linetype=Mngt), colour='blue', size = 1, vertdf)+
                        theme_bw()+
                        theme(axis.text.y = element_text(angle=90))+
                        xlab("")+
                        ylab("Vegetation Cover %")
                
                #cusum plot
                p2<- ggplot()+
                        geom_point(data=df2, aes(x=date, y=cumsum, colour=label))+
                        geom_line(data=df2, aes(x=date, y=cumsum, colour=label))+
                        scale_colour_manual(values="black",
                                            name="Cumsum \n diff \n to model",
                                            labels="")+
                        coord_cartesian(ylim = c(-1200, 1200))+       
                        geom_line(aes(x,y, linetype=Mngt), colour='blue', size = 1, vertdf)+
                        geom_line(aes(x, pos), linetype="dashed", colour='red', size=1, hordf)+
                        geom_line(aes(x, neg), linetype="dashed", colour='red', size=1, hordf)+
                        geom_hline(yintercept=0)+
                        annotate("text", min(df2[,1])+1000, hordf[1,1]+95, label = "Control Limits",
                                 size=4)+
                        theme_bw()+
                        theme(axis.text.y = element_text(angle=90))+
                        xlab("")+
                        ylab("Cumulative Sum")
                
                m <- arrangeGrob(p1,p2)
                sname.i<-sname[i]
                filename<-paste0(sname.i, "_orig_cusum",".jpeg")
                ggsave(file=filename, m)
        }
}

##Stage 2








