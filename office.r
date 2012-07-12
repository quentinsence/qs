#plot office air
#add quartiles dashed lines
#realtime LED signal when large gradient in pollution levels
#resume download rather than reloading the whole file

#### LOAD
library(RCurl)
library(maptools)
library(maps)
data(world.cities)

#personal access to data
source("local.r")

w <- read.csv("wth.txt")
#### TRANSFORM
w <- na.omit(w)

#stored as UTC, switch to BST
w$time <- w$time + 3600
#drop ip, only 1 user/collection point for now
w$ip <- NULL

w$time <- as.POSIXct(w$time,origin="1970-01-01")
t0 <- w$time[1]
t1 <- w$time[length(w$time)]
t0 <- t1 - 86400
#t0 <- as.POSIXct(paste(substr(t1,1,10),"00:00:00"))
#t1 <- t0 + 86400

#t0 <- t0 - 86400
#t1 <- t1 - 86500

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

#calculate daily maximum temperatures
w$day <- as.Date(w$time)
wmaxt <- tapply(w$temperature,w$day,max)
# max(wmaxt)
#percentage of days colder than today
hotter <- 100 * (1 - ((length(wmaxt[wmaxt >= wmaxt[length(wmaxt)]]) - 1) / dim(wmaxt)))
cat('today is hotter than',hotter,'% of the days since office move',dim(wmaxt),'days ago\n')

cat('hottest day ever',which(wmaxt == max(wmaxt),arr.ind=TRUE),wmaxt[which(wmaxt == max(wmaxt))])


#### PLOT
par(mfrow=c(4,1),mai=c(0,0.8,0,0),lab=c(10,10,7));

plot(w$gas ~ w$time,type="l",xlim=c(t0,t1),ylim=c(min(w$gas[w$time > t0]),max(w$gas[w$time > t0])),ylab="VOCs (mV)",xaxt="n")
axis.POSIXct(1, at=seq(t0,t1,by="hour"),format="%H:%M")
circadian(t0,t1)
plot(w$light ~ w$time,type="l",ylab="light (mV)",xlim=c(t0,t1))
axis.POSIXct(1, at=seq(t0,t1,by="hour"),format="%H:%M")
circadian(t0,t1)
lines(lowess(w$dust ~ w$time,f=0.2),lwd=2)
plot(w$temperature ~ w$time,type="l",ylab="temperature (C)",xlim=c(t0,t1))
axis.POSIXct(1, at=seq(t0,t1,by="hour"),format="%H:%M")
circadian(t0,t1)
plot(w$humidity ~ w$time,type="l",ylab="humidity (%Rh)",xlim=c(t0,t1))
axis.POSIXct(1, at=seq(t0,t1,by="hour"),format="%H:%M")
circadian(t0,t1)

