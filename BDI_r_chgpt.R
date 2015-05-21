



rm(list=ls())





BDI_r_chgpt <- function(dir, csv, survey, out){
        
        library(lubridate)
        library(ggplot2)
        library(dplyr)
        library(tidyr)
        library(grid)
        library(gridExtra)
        library(changepoint)
        
        ##Generic tasks
        #read data
        setwd(dir)
        df <- read.csv(csv, header = TRUE)
        df <- df[,-1]
        df[,1] <- as.Date(df[,1])
        
        #rainfall from latest downloaded zip file from BOM
        df.r <- read.csv(unz("IDCJAC0009_006011_1800.zip", "IDCJAC0009_006011_1800_Data.csv"))
        
        colnames(df.r) <- c("code", "station", "year", "month", "day", 
                            "rainfall", "period", "quality")
        
        rain.r <- df.r %>%
                select(year:rainfall) %>%
                filter(year >= year(df[1,1]) & year < year(df[length(df[,1]),1])) %>%
                group_by(year) %>%
                summarise(mthly = sum(rainfall, na.rm=TRUE))
        
        m_breaks <- seq(ymd(paste0(rain.r[1,1], "-01", "-01")),
                        ymd(paste0(tail(rain.r[,1], n=1), "-01", "-01")), by="year")
        m_labels <- as.character(m_breaks, format="%Y")
        
        sname <- names(df)[-1]
        folder <- paste0("ts-chgpt-graphs-", Sys.Date())
        folder2 <- paste0("rainfall-graphs-", Sys.Date())
        dir.create(folder)
        dir.create(folder2)
        
        #rain plot
        setwd(paste(dir,folder2, sep="\\"))
        p1 <- ggplot(rain.r, aes(x = year, y = mthly)) +
                geom_bar(stat="identity", fill="dodgerblue2")+
                labs(x = "Year", y = "Rainfall (mm)") +
                theme_bw()+
                theme(axis.text.y = element_text(angle=90))
        ggsave(file=paste0("Carnarvon Annual Rainfall", out), p1)
 i=2       
        for (i in 1:length(df[1,])){
                setwd(paste(dir,folder, sep="\\"))
                df2 <- df[, c(1, 1+i)]
                df2$label <- factor(rep("ts", by=length(df2[,1])))
                
                ##changepoint
                #make ts of data
                site.ts <- ts(df2[,2], frequency=12, 
                              start=c(as.numeric(year(df2[1,1])),
                                      as.numeric(month(df2[1,1]))),
                              end=c(as.numeric(year(tail(df2[,1], n=1))),
                                    as.numeric(month(tail(df2[,1], n=1)))))
                mvalue <- cpt.mean(site.ts, method="BinSeg")
                
                cpts <- mvalue@cpts
                
                indexer <- function(){
                        ev <- vector(mode="numeric", length=length(cpts))
                        for(j in 1:length(cpts)){
                                ev[j] <- as.numeric(df2[cpts[j],1])
                        }
                        ev
                }
                date <- c(df[1,1], as.Date(indexer()))
                
                cptdf <- data.frame(start=date[1:length(date)-1],
                                    end=date[-1], y=mvalue@param.est$mean, label=factor("mean"))
                vertdf <- data.frame(x=as.Date(survey), y=c(-Inf, Inf),
                                     Survey=factor(year(as.Date(survey))))
                
                p2 <- ggplot()+
                        geom_point(data=df2, aes(x=date, y=df2[,2], colour=label))+
                        geom_line(data=df2, aes(x=date, y=df2[,2], colour=label))+
                        scale_colour_manual(values=c("black"),
                                            name=sname[i],
                                            breaks=as.character(df2$label),
                                            labels=as.character(df2$label))+
                        geom_segment(data=cptdf, aes(x=start, y=y, xend=end, yend=y), colour="red")+
                        geom_line(aes(x,y, linetype=Survey), colour='blue', size = 0.5, vertdf)+
                        coord_cartesian(ylim = c(-10, 80))+
                        theme_bw()+
                        xlab("")+
                        ylab("Vegetation Cover %")+
                        theme(legend.justification=c(0,1), 
                              legend.position=c(0,1),
                              axis.text.y = element_text(angle=90),
                              legend.title = element_text(size = 8),
                              legend.text = element_text(size = 8)) 
                
                
                sname.i<-sname[i]
                m <- arrangeGrob(p1,p2)
                filename<-paste0(sname.i, "-rain-chgpt-plot",out)
                ggsave(file=filename, m)
        }
}

dir="Z:\\DEC\\Vegmachine_SharkBay\\Working\\Bernier_Dorre\\Working\\analysis"
csv="Bernier_Dorre_mtsd.csv"
out=".jpeg"
survey="1998-09-01"

BDI_r_chgpt(dir, csv, survey, out)
