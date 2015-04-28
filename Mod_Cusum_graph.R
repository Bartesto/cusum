####Mod_Cusum_graph###################################################################
##This function creates a specialised panel plot consisting of two time series graphs. 
##The top graph displays vegetation cover values for actual and modelled data with a 
##vertical line indicating a "management action." The bottom graph displays a cusum 
##control chart of the difference between the actual and modelled data with the "control"
##being horizontal lines indicating positive and negative 5 standard deviations. Both 
##graphs are coloured and have appropriate legends. The function also creates a folder 
##for storage of the output graphs.
## By Bart Huntley 03/07/2014 Modified for RvD's journal

## modified to include new_start - date which marks new report period to visually
## aid interpretation of cusum i.e. what has it done this period? 20/04/2015


##Ensure dir, csv and base_end and new_start is in ""
##Ensure csv name has .csv
##program defaults to 1 stdv, enter number for different stdv
##updated with new_start(date of beginning of new data for new reporting period)
rm(list=ls())

Mod_Cusum_graph<- function (dir, csv, base_end, stdv=1, new_start){
        filepath <- (dir)
        setwd(filepath)
        data <- read.csv(csv)
        data$date <- as.Date(data$date, '%d/%m/%Y') #convert to date format
        #stdv<-5 #change this to change number of std devs
        action<-as.Date(base_end, '%d/%m/%Y') #base action date
        base.index<- data[,1]<action #index the base date
        sname<- c("Site 01", "Site 02", "Site 03", "Site 04", "Site 05", "Site 06", "Site 07", "Site 08", 
                   "Site 09", "Site 10", "Site 11", "Site 12", "Site 13", "Site 14", "Site 15", "Site 16", 
                   "Site 17", "Site 18", "Site 19", "Site 21", "Site 22", "Site 23", "Site 30", "Site 31", 
                   "Site 32", "Site 33", "Site 34", "Site 35","Site 36", "Site RHR 633", "Site AGWA 657", "Site AGWA 662") #change as appropriate for headings
        dpi<-300
        dir.create("Cusum_graphs_dhi_2015")
        setwd(paste(filepath,"Cusum_graphs_dhi_2015", sep="\\"))
        

        for (i in 1:32){
                ts.i<-data[,i+1]
                mo.i<-data[,i+33]
                diff.i<-ts.i-mo.i
                diff.i[is.na(diff.i)]<-0 #converts NA's to 0
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
                dev.off()
        }
}

dir<-"Z:\\DEC\\Dirk_Hartog_Island_Ecological_Restoration\\Working\\Analysis"
csv<-"dhi_87to15_ts.csv"
base_end<-"01/01/2008"
new_start<-"12/03/2014"


Mod_Cusum_graph(dir,csv, base_end, 3, new_start)

## Plots for buffel sites with altered non standard axis (sites 17, 30, 31, 32)
## Ensure this is run separately from the above for standard plots. Clear workspace
## between running either. Suggest running for all plots first environment objects 
## may get confused.
library(dplyr)
rm(list=ls())
dir<-"Z:\\DEC\\Dirk_Hartog_Island_Ecological_Restoration\\Working\\Analysis"
csv<-"dhi_87to15_ts.csv"
base_end<-"01/01/2008"
new_start<-"12/03/2014"
filepath <- (dir)
setwd(filepath)
data <- read.csv(csv)
data$date <- as.Date(data$date, '%d/%m/%Y')

buf <- data %>%
        select(date, dhi_17, dhi_30, dhi_31, dhi_32, dhi_17_model, dhi_30_model,
               dhi_31_model, dhi_32_model)
bname<- c("Site 17", "Site 30", "Site 31", "Site 32")
action<-as.Date(base_end, '%d/%m/%Y') #base action date
base.index<- buf[,1]<action #index the base date
setwd(paste(filepath,"Cusum_graphs_dhi_2015", sep="\\"))
dpi<-300
stdv = 3

for (i in 1:4){
        ts.i<-buf[,i+1]
        mo.i<-buf[,i+5]
        diff.i<-ts.i-mo.i
        diff.i[is.na(diff.i)]<-0 #converts NA's to 0
        cu.i<-cumsum(diff.i)
        cu.base<-cu.i[base.index] #cusum for base only for sd calcs
        sName.i<-bname[i]
        FileName<-paste(sName.i, "cusum_2",".jpg",sep=" ")
        jpeg(file=FileName, width=10*dpi, height=7*dpi, res=dpi)
        par(mfrow=c(2,1)) #sets up 2 rows of graphs in 1 column
        par(mar=c(2, 4, 1.5, .5)) #manipulating margins to control blank space
        plot(mo.i ~ date, buf, col="red", ylab= NA, xlab=NA, 
             yaxt="n", xaxt="n",ylim=c(0,80))
        lines(mo.i ~ date, buf, col="red")
        abline(v=as.Date("2008-01-01"), lty=4, lwd=2) #management action date
        legend("topleft", legend=c("Landsat Time Series", "1987-2008 Baseline",
                                   "2008"), col=c("black","red", "black"), lty=c(1, 1, 4), lwd=2, cex=0.8)
        par(new=T) #allows call to plot over an existing plot
        plot(ts.i ~ date, buf, ylab= "Vegetation Cover %",
             ylim=c(0,80), main=bname[i], xlab=NA, xaxt="n")
        lines(ts.i ~ date, buf)
        plot(cu.i ~ buf$date, ylab= "Cumulative Sum", xlab= "Year",
             ylim=c(-2000,2000))
        lines(cu.i ~ buf$date)
        abline(h=stdv*sd(cu.base, na.rm=TRUE), lty=2, lwd=2, col="blue")
        abline(h=-stdv*sd(cu.base, na.rm=TRUE), lty=2, lwd=2, col="blue")
        abline(0,0)
        abline(v=action, lty=4, lwd=2) #management action date
        abline(v= as.Date(new_start, '%d/%m/%Y'), lty=4, lwd=2, col = "red") #new from
        legend("topleft", legend=c("Cumulative sum of difference to baseline model", "5 standard deviations",
                                   "2008", "New data"), col=c("black", "blue", "black", "red"),
               lty=c(1,2,4,4), lwd=2, cex=0.8)
        dev.off()
}