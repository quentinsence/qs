#TODO
#set data processing out of functions

#adjust times between office and home
#fix local clock and timezone settings
#only display radiation time series when event detected

#add ablines for quartiles
#rescale dust and pollutants

    
library(maptools)
library(maps)
library(RCurl)
library(openair)
data(world.cities)

op <- par(no.readonly = TRUE);

#wait at least 5 min between each data files update requests
if(!exists("now") || as.numeric(Sys.time()) - now > 300) {
  #local config
  source("local.r")
  k <- importKCL(site=site,met=TRUE,year=2012)
}
now <- as.numeric(Sys.time())

#home
s <- read.csv("logs.csv",header=FALSE)
#home dust
d <- read.csv("dust.csv",header=FALSE)
#home geiger
g <- read.csv("cpm.geiger.csv",header=FALSE)
#work
w <- na.omit(read.csv("wth.txt"))

#cleanup bogus timestamps above last entry
d[which(d$V1 > d$V1[length(d$V1)]),] <- NA
d <- na.omit(d)

names(s) <- c('V2','V3','V4','V5','V6','V7','V8','V9','V10')

#stored as UTC, change to local tz
s$t <- as.POSIXct(s$V2,origin="1970-01-01",tz=tz)
d$t <- as.POSIXct(d$V1,origin="1970-01-01",tz=tz)    

g$t <- as.POSIXct(g$V1+3600,origin="1970-01-01",tz=tz)  
w$t <- as.POSIXct(w$time+3600,origin="1970-01-01",tz=tz)

s$dt <- c(0,diff(s$V2))

#adjust for arduino clock inaccuracies, both feeds were picked at the same time so last timestamp should be identical in both feeds
d$t <- d$t + s$V2[length(s$V2)] - d$V1[length(d$V1)]


#drop IP address, only 1 user/collection point for now
w$ip <- NULL

#dump everything in a 24h period from 0 to 86400 seconds for hourly/time of day stats
w$time24 <- (w$time+3600) %%86400

w$weekday <- format.POSIXct(w$t,format="%w")
sweekday <- c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')

#remove tea cups before 7:30am = artefacts, morning cleaners are disrupting sensors
w$tea[w$time24 < 7.5*3600] <- 0

#boundaries to display only the last 24 hours * days
t0 <- w$t[1]
t1 <- w$t[length(w$time)]
t0 <- t1 - 86400 * 1
#t0 <- as.POSIXct(paste(substr(t1,1,10),"00:00:00"))

#get last and next alarm time
talarm1 <- t1 + s$V7[length(s$V7)]
#24h before to get previous alarm time, assuming the alarm time has not been changed since
talarm0 <- talarm1 - 86400    
#calculate daily maximum temperatures
w$day <- as.Date(w$t,origin="1970-01-01")
wmaxt <- tapply(w$temperature,w$day,max)
# max(wmaxt)
#percentage of days colder than today
hotter <- 100 * (1 - ((length(wmaxt[wmaxt >= wmaxt[length(wmaxt)]]) - 1) / dim(wmaxt)))

#should use subset to count only teas after 7am
td <- aggregate(w$tea[w$time24 > 7*3600],list(w$day[w$time24 > 7*3600]),sum)
names(td) <- c('day','tea')
td$weekday <- strftime(td$day,format="%w")
##EXTRA PLOTS
#boxplot(td$tea ~ td$weekday,names=sweekday,ylab="number of teas (250ml)",main="number of tea cups by day of the week")
#tea time distribution in 15 min chunks
#hist(w$time24[w$tea > 0]/3600,breaks=24*4)


plotqs <- function (days=1) {
    
    t0 <- t1 - 86400 * days
  
    cat('home last entry:',strftime(as.POSIXct(t1,origin="1970-01-01"),format="%Y-%m-%d %A"),'\n')
    cat('radiation max:',max(g$V2),'cpm on',strftime(as.POSIXct(g$V1[which(g$V2 == max(g$V2))],origin="1970-01-01"),format="%Y-%m-%d %A %X"),'\n')
    cat('mean radiation last 24h:',mean(g$V2[g$t > t0]),'cpm\n')    
    cat('office today is hotter than',hotter,'% of the days since office move',dim(wmaxt),'days ago\n')
    cat('office hottest day ever:',names(wmaxt[which(wmaxt == max(wmaxt),arr.ind=TRUE)]),'at',wmaxt[which(wmaxt == max(wmaxt))],'C')

    chans <- 9
    if(k$date[length(k$date)] > t0) {
      chans <- chans + 2
    }
    #plot radiation only if rare event detected
    if( max(g$V2[g$t > t0]) > 35 )  {
      chans <- chans + 1
    }
    par(mfrow=c(chans,1),mai=c(0,0.8,0,0),lab=c(10,10,7));
    
    plot(s$V4~s$t,type="l",xlim=c(t0,t1),ylim=c(min(s$V4[s$t>t0]),max(s$V4[s$t>t0])),ylab="temp (C)",xaxt="n");
    axis.POSIXct(1, at=seq(as.POSIXct(t0,origin="1970-01-01",tz=tz),as.POSIXct(t1,origin="1970-01-01",tz=tz),by="hour"),format="%H:%M")
    plot(s$V5~s$t,type="l",xlim=c(t0,t1),ylim=c(min(s$V5[s$t>t0]),max(s$V5[s$t>t0])),ylab="humidity (%Rh)",xaxt="n");
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
    if( max(g$V2[g$t > t0]) > 35 )  {
      plot(g$V2~g$t,xlim=c(t0,t1),ylab="radiation (cpm)",xaxt="n",col="gray");
      lines(lowess(g$V2[g$t > t0]~g$t[g$t > t0],f=0.1),lwd=2,col="blue")
      axis.POSIXct(1, at=seq(as.POSIXct(t0,origin="1970-01-01",tz=tz),as.POSIXct(t1,origin="1970-01-01",tz=tz),3600),format="%H:%M")
    }
    ## dust sensor + voc TGS2602 in blue, coming from arduino
    plot(d$V3~d$t,type="l",xlim=c(t0,t1),ylim=c(0,2000),ylab="dust (mV)",xaxt="n",col="gray")
    lines(lowess(d$V3[d$t > t0]~d$t[d$t > t0],f=0.01),lwd=2,col="blue")
    axis.POSIXct(1, at=seq(as.POSIXct(t0,origin="1970-01-01",tz=tz),as.POSIXct(t1,origin="1970-01-01",tz=tz),3600),format="%H:%M")
    #axis.POSIXct(1, at=seq(as.POSIXct(t0,origin="1970-01-01",tz=tz),as.POSIXct(t1,origin="1970-01-01",tz=tz),3600),format="%H:%M")
    #format.POSIXct(cat(as.POSIXct(s$V2[length(s$V2)],origin="1970-01-01",tz=tz)),format="%c")
    plot(w$gas ~ w$t,type="l",xlim=c(t0,t1),ylim=c(min(w$gas[w$t > t0]),max(w$gas[w$t > t0])),ylab="VOCs (mV)",xaxt="n")
    axis.POSIXct(1, at=seq(t0,t1,by="hour"),format="%H:%M")
    circadian(t0,t1)
    plot(w$light ~ w$t,type="l",ylab="light (mV)",xlim=c(t0,t1))
    axis.POSIXct(1, at=seq(t0,t1,by="hour"),format="%H:%M")
    circadian(t0,t1)
    abline(v=w$t[w$tea > 0 & w$t > t0],col="green")
    #lines(lowess(w$dust ~ w$t,f=0.2),lwd=2)
    plot(w$temperature ~ w$t,type="l",ylab="temperature (C)",xlim=c(t0,t1),ylim=c(min(w$temperature[w$t > t0]),max(w$temperature[w$t > t0])))
    axis.POSIXct(1, at=seq(t0,t1,by="hour"),format="%H:%M")
    circadian(t0,t1)
    plot(w$humidity ~ w$t,type="l",ylab="humidity (%Rh)",xlim=c(t0,t1),ylim=c(min(w$humidity[w$time > t0]),max(w$humidity[w$t > t0])))
    axis.POSIXct(1, at=seq(t0,t1,by="hour"),format="%H:%M")
    circadian(t0,t1)
    #KCL air quality feeds may have at least a 24h delay for data curation
    if(k$date[length(k$date)] > t0) {
        plot(k$pm10 ~ k$date,type="l",ylab=paste(site,"pm10 (C)"),xlim=c(t0,t1),ylim=c(min(k$pm10[k$date > t0 & k$pm10 != "NaN"]),max(k$pm10[k$date > t0 & k$pm10 != "NaN"])))
        axis.POSIXct(1, at=seq(t0,t1,by="hour"),format="%H:%M")
        plot(k$pm25 ~ k$date,type="l",ylab=paste(site,"pressure (mbar)"),xlim=c(t0,t1),ylim=c(min(k$pm25[k$date > t0 & k$pm25 != "NaN"]),max(k$pm25[k$date > t0 & k$pm25 != "NaN"])))
        #axis.POSIXct(1, at=seq(t0,t1,by="hour"),format="%H:%M")
    }
}

circadian <- function(t0,t1) {
    
    for (t in seq(t0,t1,86400)) {
        t <- as.POSIXct(t,origin="1970-01-01")
        #working day start and end times
        work0 <- as.POSIXct(paste(substr(t,1,10),"09:00:00"))
        work1 <- as.POSIXct(paste(substr(t,1,10),"17:30:00"))
        #sunrise sunset times
        sunrise <- sunriset(matrix(c(long,lat), nrow = 1),t,direction = "sunrise",POSIXct.out = TRUE)
        sunset  <- sunriset(matrix(c(long,lat), nrow = 1),t,direction = "sunset" ,POSIXct.out = TRUE)
        abline(v=work0,col="red")
        abline(v=work1,col="red")
        abline(v=sunrise,col="orange")
        abline(v=sunset,col="orange")
    }
}

plotmaxt <- function() {
    #plot current office temperature versus all records distribution
    par(op)
    par(mfrow=c(2,1),mai=c(0.6,0.5,0.2,0.2),lab=c(10,10,7))
    hist(wmaxt)
    abline(v=wmaxt[length(wmaxt)])
    plot(ecdf(wmaxt),xlab="office temperature")
    abline(v=wmaxt[length(wmaxt)])
}