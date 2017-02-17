###############################
#load the data
if(!file.exists('activity.csv')){
        unzip('activity.zip')
}
activityData <- read.csv('activity.csv', stringsAsFactors=FALSE)
#process/transform the data (if necesssary) into a suitable format for analysis
#change date to actual date format
activityData$date <- as.POSIXct(activityData$date, format="%Y-%m-%d")

#isolate the weekdays
activityData <- data.frame(date=activityData$date, 
                           weekday=tolower(weekdays(activityData$date)), 
                           steps=activityData$steps, 
                           interval=activityData$interval)

#check if it is weekend or weekday
activityData <- cbind(activityData, 
                      daytype=ifelse(activityData$weekday == "saturday" | 
                      activityData$weekday == "sunday", "weekend", "weekday"))

#create the data format
activityNew <- data.frame(date=activityData$date, 
                       weekday=activityData$weekday, 
                       daytype=activityData$daytype, 
                       interval=activityData$interval,
                       steps=activityData$steps)

#remove downloaded file
rm(activityData)

#what is the mean total number of steps taken per day
#ignore the missing values
#make a histohram of the total number of steps taken each day
totalData <- aggregate(activityNew$steps, by=list(activityNew$date), FUN=sum, na.rm=TRUE)

#rename heading
names(totalData) <- c("date", "total")

#plot histogram
hist(totalData$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="green", 
     xlab="Total number of steps", 
     ylim=c(0, 20), 
     main="Total number of steps taken each day")

meanData<-mean(totalData$total)
medianData<-median(totalData$total)

#calculate the means of steps for all days for each interval
meanSteps <- aggregate(activityNew$steps, by=list(activityNew$interval), 
                       FUN=mean, na.rm=TRUE)

#rename heading
names(meanSteps) <- c("interval", "mean")

#plot the time series
plot(meanSteps$interval, 
     meanSteps$mean, 
     type="l", 
     col="black", 
     lwd=2, 
     xlab="Interval (minutes)", 
     ylab="Average number of steps", 
     main="Time-series plot of the average number of steps per intervals")

#locate maximum mean
max_pos_mean <- which(meanSteps$mean == max(meanSteps$mean))

#find the value
max_interval <- meanSteps[max_pos_mean, 1]

#count number of NAs
NA.count <- sum(is.na(activityNew$steps))

#locate NA positions
NA.pos <- which(is.na(activityNew$steps))

#create a vector
meanVector <- rep(mean(activityNew$steps, na.rm=TRUE), times=length(NA.pos))

#replace NAs with the mean
activityNew[NA.pos, "steps"] <- meanVector

#calculate total number of steps
totalData2 <- aggregate(activityNew$steps, by=list(activityNew$date), FUN=sum)

#rename heading
names(totalData2) <- c("date", "total")

#calculate the histogram of the total number of steps each day
hist(totalData2$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="red", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day")

meanData2<-mean(totalData2$total)
medianData2<-median(totalData2$total)

# Load the lattice graphical library
library(lattice)

# Compute the average number of steps taken, averaged across all daytype variable
meanData3 <- aggregate(activityNew$steps, by=list(activityNew$daytype, 
                               activityNew$weekday, activityNew$interval), mean)

#rename the heading
names(meanData3) <- c("daytype", "weekday", "interval", "mean")

#plot time series
xyplot(mean ~ interval | daytype, meanData3, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))