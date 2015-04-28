
rm(list = ls(all = T))#Clears workspace if required


##Change dir as required
dir= "Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\cusum"
#dir="/Users/Bart/Documents/cusum"#MAC path
zone=51
shp="cusum.shp"
shp.ID="Point_ID"
pr=10970
#option="i35"





##STEP 1


#Landsat_stackR<- function(dir, zone, shp, shp.ID, pr, option){

        
        
##Run this section to set up variables in environment
        setwd(dir)
        ##Libraries required for spatial work
        library(raster)
        library(rgdal)
        library(maptools)
        library(sp)
        #library(tidyr)
        
        prj<-CRS(paste("+proj=utm +zone=",zone," +south +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", sep=""))
        sitesSHP <- readShapePoly(shp, IDvar = shp.ID, proj4string = prj)
        namesSHP<-rownames(sitesSHP@data)#Cool. Finds row names from IDvar supplied   
        get.list <- list.files(dir)#Normal file list
        get.list.fixed<-paste(dir,get.list,sep="\\")#Adds path
        #get.list.fixed<-paste(dir,get.list,sep="/")#Adds path Mac
        whichonesaredir <- file.info(get.list.fixed)$isdir#Determines which are folders
        dirlist <- get.list[whichonesaredir]#need this cut for grabbing dates
        dirlistLong <- get.list.fixed[whichonesaredir]#need this cut for sep filepaths
        date<-as.Date(dirlist, "%Y%m%d")
        results.a<-as.data.frame(matrix(ncol=length(namesSHP), dimnames=list(NULL, namesSHP)))
        results.b<-as.data.frame(matrix(ncol=length(namesSHP), dimnames=list(NULL, namesSHP)))
        #options<-c("i35", "ndvi", "b1", "b2", "b3", "b4", "b5", "b6")

        
## This section works on extraction and df construction
        for (i in 1:length(date)){
                #i35
                dir.i <- dirlistLong[i]
                setwd(dir.i)
                data.i<- list.files(pattern=paste('*pre.ers'))
                b3R.i<-raster(data.i, band=3)
                b4R.i<-raster(data.i, band=4)
                b5R.i<-raster(data.i, band=5)
                b3X.i<-extract(b3R.i, sitesSHP)
                b5X.i<-extract(b5R.i, sitesSHP)
                m3.i <- unlist(lapply(b3X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
                m5.i <- unlist(lapply(b5X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
                i35.i<-(m3.i+m5.i)/2
                out.a <-i35.i
                results.a <- rbind(results.a, out.a)
                
                #ndvi
                b4X.i<-extract(b4R.i, sitesSHP)
                m4.i <- unlist(lapply(b4X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
                ndvi.i<-(m4.i-m3.i)/(m4.i+m3.i)
                out.b <-ndvi.i
                results.b <- rbind(results.b, out.b)
                
        }   
setwd(dir)
results.a <- results.a[-1, ]
results.a <-cbind(date, results.a)
results.b <- results.b[-1, ]
results.b <-cbind(date, results.b)
write.csv(file=paste(pr,"i35","test.csv", sep="_"), x=results.a)
write.csv(file=paste(pr,"ndvi","test.csv", sep="_"), x=results.b)
##### Finished at this stage - export to csv for cloud QA stage
#This part will get DF in right shape   



##Code for tidying _ not sure we want this at this stage, maybe for graphing later
        setwd(dir)
        library(tidyr)#remember to detach("package:tidyr", unload=TRUE) as "extract" is masked from raster!!!
        #keep <- rowSums(is.na(results.a)) < 6 #deletes first row of NA's
        results.a <- results.a[-1, ] 
        results.a <-cbind(date, results.a)
        results.a$index <- rep("i35", length(results.a[,1]))
        results.b <- results.b[-1, ] 
        results.b <-cbind(date, results.b)
        results.b$index <- rep("ndvi", length(results.b[,1]))
        results.f <- rbind(results.a, results.b)
        results.t <- gather(results.f, "site", "value", 2:11)
        write.csv(file=paste(pr,option,"test.csv", sep="_"), x=results.a)


##uneeded from here down

ra <- results.a
ra$index <- rep("i35", length(ra[,1]))

ratest <- gather(ra, "site", "value", 2:11)
test <- gather(results.a, "site", "value", 2:11)



##Step 2

dir="W:\\usgs\\109070\\Stack"
zone=51
shp="KSCS_monitoring_plots_50m_mga51.shp"
shp.ID="Point_ID"
pr=10970
option="i35"

##Step 3

Landsat_stackR(dir, zone, shp, shp.ID, pr, option)
