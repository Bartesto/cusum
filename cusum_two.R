## cusum two ##

# This function performs a CUSUM analysis and produces a ggplot object 
# containing a time series plot with model shown above a CUSUM control chart. 
# 
# The function uses the data frame "...mtsd.csv" as the starting point.
# This data frame must be created first.

# This function calculates the model based on the regular time series
# data generated from the mtsd.csv and plots the  regular data series.
# The control chart and cusum calc is based on the regular data.


# By Bart Huntley 30/07/2015

###NOTE
# - multi dir's are not necessary in all applications
# - set project to add to graph folder name

rm(list=ls())

dir="Z:\\DEC\\Kimberley_Science_and_Sustainability_Strategy\\Working\\Mitchell_Plateau\\Analysis\\2014"
dir2="Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\cusum"
csv="kim_uu_mtsd.csv"
base_end="2008-01-01"
stdev=3
out=".pdf"
project="test"

        

cusum_two <- function(dir, csv, base_end, stdev=3, out, project){
        
        is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
        load_or_install<-function(package_names)  
        {  
                for(package_name in package_names)  
                {  
                        if(!is_installed(package_name))  
                        {  
                                install.packages(package_name,repos="http://cran.csiro.au/")  
                        }  
                        library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)  
                }  
        }  
        load_or_install(c("lubridate","ggplot2", "dplyr","tidyr", "grid", "gridExtra"))
        
        
        #REG series (mtsd)
        # Rdf is "regular data frame"
        setwd(dir)
        Rdf <- read.csv(csv, header = TRUE, stringsAsFactors=FALSE)
        Rdf <- Rdf[,-1]
        Rdf[,1] <- as.Date(Rdf[,1])
        
        setwd(dir2)
        #make base df for model creation
        base.Rdf <- Rdf %>%
                filter(date < base_end)
        sname <- names(Rdf)[-1]
        folder <- paste0("Cusum-graphs-",project,"-", Sys.Date())
        if(!file.exists(folder)){ dir.create(folder)}
        setwd(paste(dir2,folder, sep="\\"))
        
        for (i in 1:length(sname)){
                #Base period calcs and seasonal model
                b.i <- base.Rdf[,1+i]*100 #to get %
                b.ts.i <- ts(b.i, start=c(year(base.Rdf[1,1]), month(base.Rdf[1,1])),
                             end=c(year(tail(base.Rdf[,1], n=1)), month(tail(base.Rdf[,1], n=1))),
                             frequency=12)
                fit.b.i <- stl(b.ts.i, s.window="period")                          
                seasmod.b.i <- mean(b.ts.i) + as.numeric(fit.b.i$time.series[,"seasonal"])
                seasmod.b.i <- seasmod.b.i[1:12]
                
                
                #make df of site and its model
                Rdf.i <- Rdf[, c(1, 1+i)]
                Rdf.i[,2] <- Rdf.i[,2]*100
                Rdf.i$model <- rep(seasmod.b.i, length.out = length(Rdf[,1]))
                Rdf.i$cumsum <- cumsum(Rdf.i[,2]- Rdf.i[,3])
                Rdf.i$label <- factor(rep("cumsum", by=length(Rdf.i[,1])))
                Rdf2.i <- gather(Rdf.i, "Series", "Value", 2:3)
                
                
                vertdf <-data.frame(x=as.Date(base_end), y=c(-Inf, Inf),
                                    Mngt=factor(year(as.Date(base_end))))
                cu.i<-cumsum(Rdf.i[,2]- Rdf.i[,3])
                cu.base<-cu.i[1:length(b.i)]#shorten for sd calcs
                hordf <- data.frame(pos=stdev*sd(cu.base), neg=-stdev*sd(cu.base),
                                    x=c(min(Rdf.i[,1]), max(Rdf.i[,1])), Limit=factor("Limit"))
                
                
                #Model and ts plot
                p1 <- ggplot()+
                        geom_point(data=Rdf2.i, aes(x=date, y=Value, colour=Series))+
                        geom_line(data=Rdf2.i, aes(x=date, y=Value, colour=Series))+
                        scale_colour_manual(values=c("black", "red"),
                                            name="Series",
                                            breaks=as.character(Rdf2.i$Series),
                                            labels=as.character(Rdf2.i$Series))+
                        coord_cartesian(ylim = c(0, 80))+
                        geom_line(aes(x,y, linetype=Mngt), colour='blue', size = 0.5, vertdf)+
                        theme_bw()+
                        theme(axis.text.y = element_text(angle=90))+
                        xlab("")+
                        ylab("Vegetation Cover %")
                
                #cusum plot
                p2<- ggplot()+
                        geom_point(data=Rdf.i, aes(x=date, y=cumsum, colour=label))+
                        geom_line(data=Rdf.i, aes(x=date, y=cumsum, colour=label), size=0.5)+
                        scale_colour_manual(values="black",
                                            name="Cumsum \n diff \n to model",
                                            labels="")+
                        coord_cartesian(ylim = c(-1200, 1200))+       
                        geom_line(aes(x,y, linetype=Mngt), colour='blue', size = 0.5, vertdf)+
                        geom_line(aes(x, pos), linetype="dashed", colour='red', size=0.5, hordf)+
                        geom_line(aes(x, neg), linetype="dashed", colour='red', size=0.5, hordf)+
                        geom_hline(yintercept=0)+
                        annotate("text", min(Rdf.i[,1])+1000, hordf[1,1]+95, label = "Control Limits",
                                 size=2.5)+
                        theme_bw()+
                        theme(axis.text.y = element_text(angle=90))+
                        xlab("")+
                        ylab("Cumulative Sum")
                
                m <- arrangeGrob(p1,p2)
                sname.i<-sname[i]
                filename<-paste0(sname.i, "_cusum_two_", project, "_", stdev, out)
                ggsave(file=filename, m, width = 24, height = 13.5, units = "cm")
        }
}

cusum_two(dir, csv, base_end, stdev, out, project)
