#TODO
#add ablines for quartiles
#only get delta of logs when downloading
#rescale dust and pollutants
    
library(maptools)
library(maps)
library(RCurl)
data(world.cities)

#local config
source("local.r")

##########
s <- read.csv("logs.csv",header=FALSE)
#g <- read.csv("co2.csv",header=FALSE)
d <- read.csv("dust.csv",header=FALSE)
g <- read.csv("cpm.geiger.csv",header=FALSE)

names(s) <- c('V2','V3','V4','V5','V6','V7','V8','V9','V10')
#s$V1 <- NULL

s$t <- as.POSIXct(s$V2,origin="1970-01-01",tz=tz)
d$t <- as.POSIXct(d$V1,origin="1970-01-01",tz=tz)
#must fix clock and timezone settings
g$t <- as.POSIXct(g$V1+7200,origin="1970-01-01",tz=tz)

s$dt <- c(0,diff(s$V2))

#adjust for arduino clock inaccuracies, both feeds were picked at the same time so last timestamp should be identical in both feeds
#g$t <- g$t + s$V2[length(s$V2)] - g$V1[length(g$V1)]
d$t <- d$t + s$V2[length(s$V2)] - d$V1[length(d$V1)]

#s$V2 <- s$V2 - 3600;
t0 <- s$V2[1]
t1 <- s$V2[length(s$V2)] - 86400 * 0

#show only last 24 hours
t0 <- t1 - 86400
#t0 <- t1 - 60000;
#get last and next alarm time
talarm1 <- t1 + s$V7[length(s$V7)]
#24h before to get previous alarm time, assuming the alarm time has not been changed since
talarm0 <- talarm1 - 86400

par(mfrow=c(6,1),mai=c(0,0.8,0,0),lab=c(10,10,7));

plot(s$V4~s$t,type="l",xlim=c(t0,t1),ylim=c(min(s$V4[s$t>t0]),max(s$V4[s$t>t0])),ylab="temp (C)",xaxt="n");
legend("topleft",legend = paste('last: ',strftime(as.POSIXct(t1,origin="1970-01-01"),format="%Y-%m-%d %A")))
axis.POSIXct(1, at=seq(as.POSIXct(t0,origin="1970-01-01",tz=tz),as.POSIXct(t1,origin="1970-01-01",tz=tz),by="hour"),format="%H:%M")
plot(s$V5~s$t,type="l",xlim=c(t0,t1),ylim=c(30,70),ylab="humidity (%Rh)",xaxt="n");
#missed timestamps
axis.POSIXct(1, at=seq(as.POSIXct(t0,origin="1970-01-01",tz=tz),as.POSIXct(t1,origin="1970-01-01",tz=tz),3600),format="%H:%M")
plot(s$V6~s$t,type="l",xlim=c(t0,t1),ylab="light",xaxt="n");
rug(s$V2[s$V3 == 0 & s$V2 > t0],ticksize=0.06,col="blue")
#add sunrise and sunset
sunrise <- sunriset(matrix(c(long,lat), nrow = 1),as.POSIXct(t1,origin="1970-01-01",tz=tz),direction = "sunrise",POSIXct.out = TRUE)
sunset <- sunriset(matrix(c(long,lat), nrow = 1),as.POSIXct(t0,origin="1970-01-01",tz=tz),direction = "sunset",POSIXct.out = TRUE)
abline(v=sunrise$time,col="orange")
abline(v=sunset$time,col="orange")
abline(v=talarm0,col="red")
abline(v=talarm1,col="red")
axis.POSIXct(1, at=seq(as.POSIXct(t0,origin="1970-01-01",tz=tz),as.POSIXct(t1,origin="1970-01-01",tz=tz),3600),format="%H:%M")
plot(s$V9~s$t,type="l",xlim=c(t0,t1),ylab="pollutants",ylim=c(min(d$V2[d$t > t0]),max(d$V2[d$t > t0])),xaxt="n");
lines(d$V2[d$t > t0]~d$t[d$t > t0],col="blue")
rug(s$V2[s$dt > 310 & s$V2 > t0],ticksize=0.2,col="red")
rug(s$V2[s$V2 > t0],ticksize=0.1)
axis.POSIXct(1, at=seq(as.POSIXct(t0,origin="1970-01-01",tz=tz),as.POSIXct(t1,origin="1970-01-01",tz=tz),3600),format="%H:%M")
plot(g$V2~g$t,xlim=c(t0,t1),ylab="radiation (cpm)",xaxt="n",col="gray");
lines(lowess(g$V2[g$t > t0]~g$t[g$t > t0]),lwd=2,col="blue")
axis.POSIXct(1, at=seq(as.POSIXct(t0,origin="1970-01-01",tz=tz),as.POSIXct(t1,origin="1970-01-01",tz=tz),3600),format="%H:%M")
## dust sensor + voc TGS2602 in blue, coming from arduino
plot(d$V3~d$t,type="l",xlim=c(t0,t1),ylim=c(0,2000),ylab="dust (mV)",xaxt="n",col="gray")
lines(lowess(d$V3[d$t > t0]~d$t[d$t > t0],f=0.01),lwd=4,col="blue")
axis.POSIXct(1, at=seq(as.POSIXct(t0,origin="1970-01-01",tz=tz),as.POSIXct(t1,origin="1970-01-01",tz=tz),3600),format="%H:%M")
#axis.POSIXct(1, at=seq(as.POSIXct(t0,origin="1970-01-01",tz=tz),as.POSIXct(t1,origin="1970-01-01",tz=tz),3600),format="%H:%M")
#format.POSIXct(cat(as.POSIXct(s$V2[length(s$V2)],origin="1970-01-01",tz=tz)),format="%c")
