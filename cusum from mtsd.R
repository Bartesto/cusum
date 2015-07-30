## cusum from mtsd ##

# This function performs the original CUSUM analysis and produces a ggplot object 
# containing a time series plot with model shown above a CUSUM control chart. 
# 
# The function uses the data frame "...mtsd.csv" as the starting point.
# This data frame must be created first.

# This function now calculates the model based on the regular time series
# data generated from the mtsd.csv but plots the raw irregular data series.
# The control chart and cusum calc is based on the irregular data.

 
# By Bart Huntley 30/07/2015

###NOTE
# - multi dir's are not necessary in all applications
# - set project to add to graph folder name

rm(list=ls())

dir="Z:\\DEC\\Kimberley_Science_and_Sustainability_Strategy\\Working\\Mitchell_Plateau\\Analysis\\2014"
dir2="Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\cusum"
csv="kim_uu_mtsd.csv"
csv2="Landsat_data_for_monitoring_plots_86to14_170215_Uunguu.csv"
base_end="2008-01-01"
stdev=3
out=".pdf"
project="project"

i=3

oca <- function(dir, csv, base_end, stdev=3, out, project){
        
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
        setwd(dir)
        Rdf <- read.csv(csv, header = TRUE, stringsAsFactors=FALSE)
        Rdf <- Rdf[,-1]
        Rdf[,1] <- as.Date(Rdf[,1])
        
        #IRREG series
        #section to handle raw data but summarise by month
        #leaves NaN in but dates match interpolated df2 data
        Idf <- read.csv(csv2, header = TRUE, stringsAsFactors = FALSE)
        Idf$date <- dmy(Idf$date)#create date here to order by
        Idf <- Idf[order(Idf$date),]#Very Important - order it!
        all <- seq(Idf[1,1], tail(Idf[,1], n=1), by = 'days') #daily date sequence for whole period
        d <- rep('NA', length(all)) #dummy data column for data frame
        alldates <- data.frame(date = all, val = d)#make df of complete date seq
        Idf2 <- left_join(alldates, Idf, by = "date")#join datasets for whole seq
        Idf2 <- Idf2[,-2]#drop dummy column
        Idf3 <- Idf2 %>%
                mutate(month = month(date), year = year(date)) %>%
                group_by(year, month) %>%
                summarise_each(funs(mean(., na.rm = TRUE)))
        Idf3 <- as.data.frame(Idf3)
        
        ##At this point the REG and IRREG series are comparable - same length
        
        setwd(dir2)
        #make base df
        base.Rdf <- Rdf %>%
                filter(date < base_end)
        sname <- names(Rdf)[-1]
        folder <- paste0("Cusum-graphs-",project,"-", Sys.Date())
        if(!file.exists(folder)){ dir.create(folder)}
        setwd(paste(dir2,folder, sep="\\"))
        
      
        for (i in 1:length(sname)){
                #Base period calcs
                b.i <- base.Rdf[,1+i]*100 #to get %
                b.ts.i <- ts(b.i, start=c(year(base.Rdf[1,1]), month(base.Rdf[1,1])),
                             end=c(year(tail(base.Rdf[,1], n=1)), month(tail(base.Rdf[,1], n=1))),
                             frequency=12)
                fit.b.i <- stl(b.ts.i, s.window="period")                          
                seasmod.b.i <- mean(b.ts.i) + as.numeric(fit.b.i$time.series[,"seasonal"])
                seasmod.b.i <- seasmod.b.i[1:12]
                
                
                ##New data for cusum plot
                cusdf.i <- Rdf[, c(1, 1+i)]
                cusdf.i[,2] <- Idf3[,3+i]*100
                cusdf.i$model <- rep(seasmod.b.i, length.out = length(Rdf[,1]))
                cusdf.i$cumsum <- cusdf.i[,2]- cusdf.i[,3]#calc new diff to model using IRREG
                cusdf.i[,4][is.nan(cusdf.i[,4])] <- 0#replace NaN's so cumsum works
                cusdf.i[,4] <- cumsum(cusdf.i[,4])#run cumsum
                
                ##New data for first plot
                tsdf.i <- gather(cusdf.i, "Series", "Value", 2:3)
                
                Rdf.i[,2] <- Rdf.i[,2]*100
                Rdf.i$model <- rep(seasmod.b.i, length.out = length(Rdf[,1]))#repeats 12 mth model
                Rdf.i$cumsum <- cumsum(Rdf.i[,2]- Rdf.i[,3])
                #Rdf.i$label <- factor(rep("cumsum", by=length(df2[,1])))
                
                df2 <- df[, c(1, 1+i)]
                df2[,2] <- df2[,2]*100
                df2$model <- rep(seasmod.b.i, length.out = length(df[,1]))#repeats 12 mth model
                df2$cumsum <- cumsum(df2[,2]-df2[,3])
                df2$label <- factor(rep("cumsum", by=length(df2[,1])))
                
                df3 <- gather(df2, "Series", "Value", 2:3)
                
                
                
                #Create new df for first panel plot so that model and non-interpolated data can be
                #plotted
                df4 <- Rdf.i #make copy of original
                df4[,2] <- Idf3[,3+i]*100 #add in IRREG series (and convert to %)
                df4[,4] <- df4[,2]-df4[,3]#calc new diff to model using IRREG
                df4[,4][is.nan(df4[,4])] <- 0#replace NaN's so cumsum works
                df4[,4] <- cumsum(df4[,4])#run cumsum
                
                df5 <- gather(df4, "Series", "Value", 2:3)
                
                #make df of vertical (mngt action) line info
                vertdf <-data.frame(x=as.Date(base_end), y=c(-Inf, Inf),
                                    Mngt=factor(year(as.Date(base_end))))
                #make df of horizontal control limits
                cu.i<-cusdf.i[,4]#irreg cumsum
                cu.base<-cu.i[1:length(b.i)]#shorten for sd calcs
                hordf <- data.frame(pos=stdev*sd(cu.base), neg=-stdev*sd(cu.base),
                                    x=c(min(cusdf.i[,1]), max(cusdf.i[,1])), 
                                    Limit=factor("Limit"))
                
                
                #Model and ts plot
#                 p1 <- ggplot()+
#                         geom_point(data=df3, aes(x=date, y=Value, colour=Series))+
#                         geom_line(data=df3, aes(x=date, y=Value, colour=Series))+
#                         scale_colour_manual(values=c("black", "red"),
#                                             name="Series",
#                                             breaks=as.character(df3$Series),
#                                             labels=as.character(df3$Series))+
#                         coord_cartesian(ylim = c(0, 100))+
#                         geom_line(aes(x,y, linetype=Mngt), colour='blue', size = 0.5, vertdf)+
#                         theme_bw()+
#                         theme(axis.text.y = element_text(angle=90))+
#                         xlab("")+
#                         ylab("Vegetation Cover %")
#                 
#                 #cusum plot
#                 p2<- ggplot()+
#                         geom_point(data=df2, aes(x=date, y=cumsum, colour=label))+
#                         geom_line(data=df2, aes(x=date, y=cumsum, colour=label), size=0.5)+
#                         scale_colour_manual(values="black",
#                                             name="Cumsum \n diff \n to model",
#                                             labels="")+
#                         coord_cartesian(ylim = c(-1200, 1200))+       
#                         geom_line(aes(x,y, linetype=Mngt), colour='blue', size = 0.5, vertdf)+
#                         geom_line(aes(x, pos), linetype="dashed", colour='red', size=0.5, hordf)+
#                         geom_line(aes(x, neg), linetype="dashed", colour='red', size=0.5, hordf)+
#                         geom_hline(yintercept=0)+
#                         annotate("text", min(df2[,1])+1000, hordf[1,1]+95, label = "Control Limits",
#                                  size=2.5)+
#                         theme_bw()+
#                         theme(axis.text.y = element_text(angle=90))+
#                         xlab("")+
#                         ylab("Cumulative Sum")
#                 
#                 m <- arrangeGrob(p1,p2)
#                 sname.i<-sname[i]
#                 filename<-paste0(sname.i, "_orig_cusum_wil_", stdev, out)
#                 ggsave(file=filename, m, width = 24, height = 13.5, units = "cm")
                
                
                
                
                p3<- ggplot()+
                        geom_point(data=tsdf.i, aes(x=date, y=Value, colour=Series))+
                        geom_line(data=tsdf.i, aes(x=date, y=Value, colour=Series))+
                        scale_colour_manual(values=c("black", "red"),
                                            name="Series",
                                            breaks=as.character(df3$Series),
                                            labels=as.character(df3$Series))+
                        coord_cartesian(ylim = c(0, 100))+
                        geom_line(aes(x,y, linetype=Mngt), colour='blue', size = 0.5, vertdf)+
                        theme_bw()+
                        theme(axis.text.y = element_text(angle=90))+
                        xlab("")+
                        ylab("Vegetation Cover %")
                
                p4<- ggplot()+
                        geom_point(data=cusdf.i, aes(x=date, y=cumsum, colour=label))+
                        geom_line(data=cusdf.i, aes(x=date, y=cumsum, colour=label), size=0.5)+
                        scale_colour_manual(values="black",
                                            name="Cumsum \n diff \n to model",
                                            labels="")+
                        coord_cartesian(ylim = c(-1200, 1200))+       
                        geom_line(aes(x,y, linetype=Mngt), colour='blue', size = 0.5, vertdf)+
                        geom_line(aes(x, pos), linetype="dashed", colour='red', size=0.5, hordf)+
                        geom_line(aes(x, neg), linetype="dashed", colour='red', size=0.5, hordf)+
                        geom_hline(yintercept=0)+
                        annotate("text", min(cusdf.i[,1])+1000, hordf[1,1]+95, label = "Control Limits",
                                 size=2.5)+
                        theme_bw()+
                        theme(axis.text.y = element_text(angle=90))+
                        xlab("")+
                        ylab("Cumulative Sum")
                
                o <- arrangeGrob(p3,p4)
                sname.i<-sname[i]
                filename<-paste0(sname.i, "_cusum_uu_", stdev, out)
                ggsave(file=filename, o, width = 24, height = 13.5, units = "cm")
                
#                 df4$s_cumsum <- df2[,4]
#                 df4 <- df4[,-5]
#                 df6 <- gather(df4, "Method", "Sum", 4:5)
#                 
#                 p5<- ggplot()+
#                         geom_point(data=df6, aes(x=date, y=Sum, colour=Method))+
#                         geom_line(data=df6, aes(x=date, y=Sum, colour=Method))+
#                         scale_colour_manual(values=c("black", "orange"),
#                                                 name="Method",
#                                                 breaks=as.character(df6$Method),
#                                                 labels=as.character(df6$Method))+
# #                         scale_colour_manual(values="black",
# #                                             name="Cumsum \n diff \n to model",
# #                                             labels="")+
#                         
# #                         geom_point(data=df2, aes(x=date, y=cumsum), colour="blue")+
# #                         geom_line(data=df2, aes(x=date, y=cumsum), colour="blue", size=0.5)+
# #                         scale_colour_manual(values="green",
# #                                             name="Cumsum \n diff \n to model",
# #                                             labels="")+
#                         
#                         coord_cartesian(ylim = c(-1200, 1200))+       
#                         geom_line(aes(x,y, linetype=Mngt), colour='blue', size = 0.5, vertdf)+
#                         geom_line(aes(x, pos), linetype="dashed", colour='red', size=0.5, hordf)+
#                         geom_line(aes(x, neg), linetype="dashed", colour='red', size=0.5, hordf)+
#                         geom_hline(yintercept=0)+
#                         annotate("text", min(df2[,1])+1000, hordf[1,1]+95, label = "Control Limits",
#                                  size=2.5)+
#                         theme_bw()+
#                         theme(axis.text.y = element_text(angle=90))+
#                         xlab("")+
#                         ylab("Cumulative Sum")
# 
#                         filename<-paste0(sname.i, "_orig_cusum_wil_comparison_", stdev, out)
#                         ggsave(file=filename, p5, width = 24, height = 13.5, units = "cm")
                

        }
}

oca(dir, csv, base_end, stdev, out, project)
