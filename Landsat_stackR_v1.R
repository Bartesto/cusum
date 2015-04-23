#test
rm(list = ls(all = T))#Clears workspace if required






##STEP 1


Landsat_stackR<- function(dir, zone, shp, shp.ID, pr, option){
        
        setwd(dir)
        ##Libraries required for spatial work
        library(raster)
        library(rgdal)
        library(maptools)
        library(sp)
        
        prj<-CRS(paste("+proj=utm +zone=",zone," +south +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", sep=""))
        sitesSHP <- readShapePoly(shp, IDvar = shp.ID, proj4string = prj)
        namesSHP<-rownames(sitesSHP@data)#Cool. Finds row names from IDvar supplied   
        get.list <- list.files(dir)#Normal file list
        get.list.fixed<-paste(dir,get.list,sep="\\")#Adds path
        whichonesaredir <- file.info(get.list.fixed)$isdir#Determines
        dirlist <- get.list[whichonesaredir]
        dirlistLong <- get.list.fixed[whichonesaredir]
        date<-as.Date(dirlist, "%Y%m%d")
        results.a<-as.data.frame(matrix(ncol=length(namesSHP), dimnames=list(NULL, namesSHP)))
        options<-c("i35", "ndvi", "b1", "b2", "b3", "b4", "b5", "b6")
        
        for (i in 1:length(date)){
                
                dir.i <- dirlistLong[i]
                setwd(dir.i)
                data.i<- list.files(pattern=paste('*pre.ers'))
                b1R.i<-raster(data.i, band=1)
                b2R.i<-raster(data.i, band=2)
                b3R.i<-raster(data.i, band=3)
                b4R.i<-raster(data.i, band=4)
                b5R.i<-raster(data.i, band=5)
                b6R.i<-raster(data.i, band=6)
                if(option == "i35"){
                        b3X.i<-extract(b3R.i, sitesSHP)
                        b5X.i<-extract(b5R.i, sitesSHP)
                        m3.i <- unlist(lapply(b3X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
                        m5.i <- unlist(lapply(b5X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
                        i35.i<-(m3.i+m5.i)/2
                        out<-i35.i
                } else if (option == "ndvi"){
                        b3X.i<-extract(b3R.i, sitesSHP)
                        b4X.i<-extract(b4R.i, sitesSHP)
                        m3.i <- unlist(lapply(b3X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
                        m4.i <- unlist(lapply(b4X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
                        ndvi.i<-(m4.i-m3.i)/(m4.i+m3.i)
                        out<-ndvi.i
                } else if (option == "b1"){
                        b1X.i<-extract(b1R.i, sitesSHP)
                        m1.i <- unlist(lapply(b1X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
                        out<-m1.i
                } else if (option == "b2"){
                        b2X.i<-extract(b2R.i, sitesSHP)
                        m2.i <- unlist(lapply(b2X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
                        out<-m2.i
                } else if (option == "b3"){
                        b3X.i<-extract(b3R.i, sitesSHP)
                        m3.i <- unlist(lapply(b3X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
                        out<-m3.i
                } else if (option == "b4"){
                        b4X.i<-extract(b4R.i, sitesSHP)
                        m4.i <- unlist(lapply(b4X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
                        out<-m4.i
                } else if (option == "b5"){
                        b5X.i<-extract(b5R.i, sitesSHP)
                        m5.i <- unlist(lapply(b5X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
                        out<-m5.i
                } else {
                        b6X.i<-extract(b6R.i, sitesSHP)
                        m6.i <- unlist(lapply(b6X.i, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))
                        out<-m6.i
                }              
                
                
                results.a<- rbind(results.a, out)
                
        }       
        setwd(dir)
        #keep <- rowSums(is.na(results.a)) < 6 #deletes first row of NA's
        results.a <- results.a[-1, ] 
        results.a<-cbind(date, results.a)
        write.csv(file=paste(pr,option,"test.csv", sep="_"), x=results.a)
}

##Step 2

dir="W:\\usgs\\109070\\Stack"
zone=51
shp="KSCS_monitoring_plots_50m_mga51.shp"
shp.ID="Point_ID"
pr=10970
option="i35"

##Step 3

Landsat_stackR(dir, zone, shp, shp.ID, pr, option)

