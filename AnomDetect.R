devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
devtools::install_github("twitter/BreakoutDetection")
library(BreakoutDetection)
library(changepoint)

#Twitter anomaly detection 
data(raw_data)

res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='pos', plot=TRUE)
res$plot

#Twitter breakout detection
data(Scribe)
sc = breakout(Scribe, min.size=24, method='multi', beta=.001, degree=1, plot=TRUE)
sc$plot




#Anomaly on dhi data - not working
site17_AD <- df3[,c(3, 20)]
site17_AD$dhi_17 <- na.approx(site17_AD[,2])
res1 = AnomalyDetectionTs(site17_AD, max_anoms=0.02, direction='pos', plot=TRUE)
res1$plot

#Breakout on dhi data - working
site17_ADnt <- as.numeric(site17_AD$dhi_17)
sc1 = breakout(site17_ADnt, min.size=24, method='multi', beta=.001, degree=1, plot=TRUE)
sc1$plot

names(site17_AD) <- c("timestamp", "count")
sc2 = breakout(site17_AD, min.size=24, method='multi', beta=.001, degree=1, plot=TRUE)
sc2$plot

#Changepoint
site17_ADnt_TS <- ts(site17_ADnt, frequency=12, start=c(1987, 2), end=c(2015, 10))
plot(site17_ADnt_TS)
mvalue = cpt.mean(site17_ADnt_TS, method="BinSeg")
cpts(mvalue)
plot(mvalue)
vvalue = cpt.var(diff(site17_ADnt_TS), method="SegNeigh", Q=6)
cpts(vvalue)
plot(vvalue)
par(mfrow=c(2,1))
plot(site17_ADnt_TS)
plot(vvalue)
d=decompose(site17_ADnt_TS)
plot(d)


