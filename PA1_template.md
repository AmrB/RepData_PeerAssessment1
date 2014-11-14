---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
# the csv file needs to be in the working directory
actvt<- read.csv("activity.csv", header=TRUE, sep=",")
# get the number of days in the dataset - there are 61 days
length(unique(actvt$date))
# get each day's observations
table(actvt$date, useNA="ifany") 
# check the classes of variables.
sapply(actvt[1,], class)
# change the "steps" variable class to Numeric
actvt$steps <- as.numeric(actvt$steps) 
# change the "interval" variable class to Numeric
actvt$interval <- as.numeric(actvt$interval) 
# new data frame after removing NAs.
actvtData<- actvt[complete.cases(actvt),] 
```

```
## [1] 61
```

```
## 
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##        288        288        288        288        288        288 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##        288        288        288        288        288        288 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##        288        288        288        288        288        288 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##        288        288        288        288        288        288 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##        288        288        288        288        288        288 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##        288        288        288        288        288        288 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##        288        288        288        288        288        288 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##        288        288        288        288        288        288 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##        288        288        288        288        288        288 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##        288        288        288        288        288        288 
## 2012-11-30 
##        288
```

```
##     steps      date  interval 
## "integer"  "factor" "integer"
```


## What is mean total number of steps taken per day?


```r
# get the total number of steps per day:
stps<- ddply(actvtData, c("date"), function (x) apply(x[1], 2, sum)) 
hist(stps$steps, xlab= "Total Number of Steps/Day",
      main="Histogram of the Total Number of Steps Taken Each Day",
      col= "red")
```

![plot of chunk Hist mean/median of steps/day](figure/Hist mean/median of steps/day.png) 

```r
# get the mean of total steps
mean(stps$steps, na.rm=TRUE) 
# get the median of total steps
median(stps$steps, na.rm=TRUE) 
```

```
## [1] 10766
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
# Calculating the mean of steps related to each interval:
mnsteps <- ddply(actvtData, c("interval"), function (x) apply(x[1], 2, mean))

# Creating the panel plot containing a time series plot of the 5-minute interval 
# and the average number of steps taken, averaged across all days:
xyplot(mnsteps$steps ~ mnsteps$interval, 
       type= "l", 
       ylab="Mean Steps",
       xlab= "Intervals", 
       main="Average Daily Activity Pattern",
       lwd=1.5)
```

![plot of chunk time series plot](figure/time series plot.png) 

```r
# get the maximum amount of mean steps (it's 206.1698)       
max(mnsteps$steps) 
# get the interval related to the maximum of mean step (it's interval 835):
mnsteps[mnsteps$steps==max(mnsteps$steps),] 
```

```
## [1] 206.2
```

```
##     interval steps
## 104      835 206.2
```


## Imputing missing values


```r
sum(is.na(actvt$interval)) # without NA.
sum(is.na(actvt$date)) # without NA.
sum(is.na(actvt$steps)) # There are 2304 rows with NAs.

#replacing the steps of the rows having NAs with the mean steps 
#for each 5-min interval across the all data:
mnsteps <- ddply(actvtData, c("interval"), function (x) apply(x[1], 2, mean))
newData<- actvt
newData[1:288, 1] <- mnsteps$steps 
newData[2017:2304, 1]<- mnsteps$steps
newData[8929:9216, 1]<- mnsteps$steps
newData[9793:10080, 1]<- mnsteps$steps
newData[11233:11520, 1]<- mnsteps$steps
newData[11521:11808, 1]<- mnsteps$steps
newData[12673:12960, 1]<- mnsteps$steps
newData[17281:17568, 1]<- mnsteps$steps
sum(is.na(newData)) # There is no NA.
#Calculating the sum of steps related to each day:
newsm<- ddply(newData, c("date"), function (x) apply(x[1], 2, sum))

#Creating the histogram of the total number of steps taken each day
#This histogram is slightly different from the previous one which was  
#created before filling the NAs. This histogram has higher frequency around mean steps: 
hist(newsm$steps, xlab= "Total Number of Steps/Day", 
     main="Total Number of Steps Taken Each Day", 
     col= "blue")
```

![plot of chunk NAs](figure/NAs.png) 

```r
#obtaining the mean of total steps after filling in all NAs:
#the mean is exactly the same as before.
#Imputing missing data did not have majore impact on 
#the estimates of the total daily number of steps.
mean(newsm$steps) # 10766.19
#obtaining the median of total steps after filling in all NAs:
#The median was increased very slightly and is now equal to the mean.
#Imputing missing data did not have majore impact on 
#the estimates of the total daily number of steps.
median(newsm$steps) # 10766.19
```

```
## [1] 0
```

```
## [1] 0
```

```
## [1] 2304
```

```
## [1] 0
```

```
## [1] 10766
```

```
## [1] 10766
```

## Are there differences in activity patterns between weekdays and weekends?

```r
# Adding new column to the dataset "newData" containing the week days'names:
newData$WD <- weekdays(as.Date(newData$date))
# Replacing the names of week day with "Weekday" and "Weekend":
newData$WD[newData$WD == "Friday"] <- "Weekday"
newData$WD[newData$WD == "Monday"] <- "Weekday"
newData$WD[newData$WD == "Thursday"] <- "Weekday"
newData$WD[newData$WD == "Tuesday"] <- "Weekday"
newData$WD[newData$WD == "Wednesday"] <- "Weekday"
newData$WD[newData$WD == "Saturday"] <- "Weekend"
newData$WD[newData$WD == "Sunday"] <- "Weekend"
table(newData$WD)

#Creating the panel plot containing a time series plot of the 5-min interval
#and the average number of steps taken, averaged across all 
#weekday days or weekend days.
newmnsteps<- ddply(newData, c("interval", "WD"), function(x) apply(x[1], 2, mean))
xyplot(newmnsteps$steps ~ newmnsteps$interval | newmnsteps$WD, 
       type="l", ylab="Number of Steps", xlab="Interval",
       main="Plot of Interval vs. Number of Steps", layout=c(1,2))
```

![plot of chunk the days of week](figure/the days of week.png) 

```
## 
## Weekday Weekend 
##   12960    4608
```
