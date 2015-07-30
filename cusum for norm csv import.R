# cusum for norm csv import
# This function performs the original CUSUM analysis and produces a ggplot object 
# containing a time series plot with model shown above a CUSUM control chart. 
# 
# The function uses the data frame "...mtsd.csv" as the starting point.
# This data frame must be created first.

# This function now calculates the model based on the regular time series
# data generated from the mtsd.csv but plots the raw irregular data series
# and the control chart and cucum is based on the irregular data.

 
# By Bart Huntley 30/07/2015

rm(list=ls())

dir="Z:\\DEC\\Kimberley_Science_and_Sustainability_Strategy\\Working\\Mitchell_Plateau\\Analysis\\2014"
dir2="Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\cusum"
csv="kim_uu_mtsd.csv"
csv2="Landsat_data_for_monitoring_plots_86to14_170215_Uunguu.csv"
base_end="2008-01-01"
stdev=3
out=".pdf"
project="project"


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
        
        date_df_fix <- function(dir,csv){
                setwd(dir)
                
                df <- read.csv(csv, header = TRUE)
                df$date <- as.Date(paste0(as.character(df[,1]), "/", as.character(df[,2]), "/", 
                                          as.character(df[,3])), "%d/%m/%Y")#concat d m Y
                df <- df[,-1:-3]#remove d m Y cols
                l <- length(colnames(df))#total length
                end <- l-1#length without date
                df <- df[, c(7, 1:6)]#re-order df
                return(df)
        }
        
        
        #REG series
        setwd(dir)
        df <- read.csv(csv, header = TRUE, stringsAsFactors=FALSE)
        df <- df[,-1]
        df[,1] <- as.Date(df[,1])
        
        #IRREG series
        #section to handle raw data but summarise by month
        #leaves NaN in but dates match interpolated df2 data
        ldf <- read.csv(csv2, header = TRUE, stringsAsFactors = FALSE)
        ldf$date <- dmy(ldf$date)#create date here to order by
        ldf <- ldf[order(ldf$date),]#Very Important - order it!
        all <- seq(ldf[1,1], tail(ldf[,1], n=1), by = 'days') #daily date sequence for whole period
        d <- rep('NA', length(all)) #dummy data column for data frame
        alldates <- data.frame(date = all, val = d)#make df of complete date seq
        ldf2 <- left_join(alldates, ldf, by = "date")#join datasets for whole seq
        ldf2 <- ldf2[,-2]#drop dummy column
        ldf3 <- ldf2 %>%
                mutate(month = month(date), year = year(date)) %>%
                group_by(year, month) %>%
                summarise_each(funs(mean(., na.rm = TRUE)))
        ldf3 <- as.data.frame(ldf3)
        
        ##At this point the REG and IRREG series are comparable - same length
        
        setwd(dir2)
        #make base df
        base.df <- df %>%
                filter(date < base_end)
        sname <- names(df)[-1]
        folder <- paste0("Cusum-graphs-",project,"-", Sys.Date())
        if(!file.exists(folder)){ dir.create(folder)}
        setwd(paste(dir2,folder, sep="\\"))
        
      
        for (i in 1:length(sname)){
                #Base period calcs
                b.i <- base.df[,1+i]*100 #to get %
                b.ts.i <- ts(b.i, start=c(year(base.df[1,1]), month(base.df[1,1])),
                             end=c(year(tail(base.df[,1], n=1)), month(tail(base.df[,1], n=1))),
                             frequency=12)
                fit.b.i <- stl(b.ts.i, s.window="period")                          
                seasmod.b.i <- mean(b.ts.i) + as.numeric(fit.b.i$time.series[,"seasonal"])
                seasmod.b.i <- seasmod.b.i[1:12]
                
                
                #make df of site and its model for smoothed (interpolated) data
                df2 <- df[, c(1, 1+i)]
                df2[,2] <- df2[,2]*100
                df2$model <- rep(seasmod.b.i, length.out = length(df[,1]))#repeats 12 mth model
                df2$cumsum <- cumsum(df2[,2]-df2[,3])
                df2$label <- factor(rep("cumsum", by=length(df2[,1])))
                
                df3 <- gather(df2, "Series", "Value", 2:3)
                
                
                
                #Create new df for first panel plot so that model and non-interpolated data can be
                #plotted
                df4 <- df2 #make copy of original
                df4[,2] <- ldf3[,3+i]*100 #add in IRREG series (and convert to %)
                df4[,4] <- df4[,2]-df4[,3]#calc new diff to model using IRREG
                df4[,4][is.nan(df4[,4])] <- 0#replace NaN's so cumsum works
                df4[,4] <- cumsum(df4[,4])#run cumsum
                
                df5 <- gather(df4, "Series", "Value", 2:3)
                
                #make df of vertical (mngt action) line info
                vertdf <-data.frame(x=as.Date(base_end), y=c(-Inf, Inf),
                                    Mngt=factor(year(as.Date(base_end))))
                #make df of horizontal control limits
                cu.i<-df4[,4]#irreg cumsum
                cu.base<-cu.i[1:length(b.i)]#shorten for sd calcs
                hordf <- data.frame(pos=stdev*sd(cu.base), neg=-stdev*sd(cu.base),
                                    x=c(min(df2[,1]), max(df2[,1])), Limit=factor("Limit"))
                
                
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
                        geom_point(data=df5, aes(x=date, y=Value, colour=Series))+
                        geom_line(data=df5, aes(x=date, y=Value, colour=Series))+
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
                        geom_point(data=df4, aes(x=date, y=cumsum, colour=label))+
                        geom_line(data=df4, aes(x=date, y=cumsum, colour=label), size=0.5)+
                        scale_colour_manual(values="black",
                                            name="Cumsum \n diff \n to model",
                                            labels="")+
                        coord_cartesian(ylim = c(-1200, 1200))+       
                        geom_line(aes(x,y, linetype=Mngt), colour='blue', size = 0.5, vertdf)+
                        geom_line(aes(x, pos), linetype="dashed", colour='red', size=0.5, hordf)+
                        geom_line(aes(x, neg), linetype="dashed", colour='red', size=0.5, hordf)+
                        geom_hline(yintercept=0)+
                        annotate("text", min(df2[,1])+1000, hordf[1,1]+95, label = "Control Limits",
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
