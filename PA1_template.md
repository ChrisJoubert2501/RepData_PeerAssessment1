# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The data is loaded using the read.csv() function (and the firste few lines are shown).

```r
activity <- read.csv("activity/activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?
The step data is aggregated by date and a histogram is plotted. 

```r
SumSteps <- aggregate(.~date, data = activity[,c(1,2)], sum)
hist(SumSteps$steps,main = "Histogram of the number of steps taken per day",breaks=20,xlab = "Number of Steps",ylim = c(0,20))
```

![](PA1_template_files/figure-html/MeanStepsHist-1.png) 


The mean and median is then calculated.

```r
MeanSteps <- mean(SumSteps$steps)
MedianSteps <- median(SumSteps$steps)
```

The mean number of steps per day is 10766.19.  
The median number of steps per day is 10765.

## What is the average daily activity pattern?
The step data is aggregated by the 5-min interval identifier and the daily pattern is plotted.

```r
Avg5MinInt <- aggregate(.~interval, data = activity[,c(1,3)], mean)
plot(y=Avg5MinInt$steps,x=paste( Avg5MinInt$interval),type="l", xlab = "5-Min Interval Identifier",ylab = "Average number of Steps")
title("Daily Step Pattern")
```

![](PA1_template_files/figure-html/DailyPattern-1.png) 

The 5-min interval with the maximum average number of steps is then determined.

```r
MaxDailyPattern <- Avg5MinInt$interval[ which.max(Avg5MinInt$steps) ]
```
The maximum average number of steps occurs in the 835th 5-Min interval.


## Imputing missing values

The number of NA records are determined

```r
NA_Count <- sum(is.na(activity$steps))
```
The total number of NA records are 2304.  


The NA records are replaced with the average for that 5-Min interval (in a new dataframe called "activity2").

```r
activity2 <- activity
for (i in 1:nrow(activity2[is.na(activity$steps),])){
  activity2[is.na(activity$steps),][i,1]<- 
    Avg5MinInt[Avg5MinInt$interval == activity2[is.na(activity$steps),][i,3],2]
}

SumSteps2 <- aggregate(.~date, data = activity2[,c(1,2)], sum)
hist(SumSteps2$steps,main = "Histogram of the number of steps taken per day",breaks=20,xlab = "Number of Steps", ylim = c(0,20))
```

![](PA1_template_files/figure-html/NA_Replace-1.png) 


The mean and median is then calculated again.

```r
MeanSteps2 <- mean(SumSteps2$steps)
MedianSteps2 <- median(SumSteps2$steps)
```

The mean number of steps per day is 10766.19.  
The median number of steps per day is 10766.19.
It is clear that imputing the missing data with the average of the specific 5-min interval has not affected the mean and median of the data. It only increased the number of days that has the average number of steps (this is clear from the histogram and it is to be expected).



## Are there differences in activity patterns between weekdays and weekends?
A weekday/weekend factor variable is added to the dataframe ("activity2")

```r
activity2$DayType <- "weekday"
activity2[weekdays(as.POSIXct(activity2$date)) %in% c("Saturday","Sunday"),]$DayType <- "weekend"
```

The step data is aggregated by the 5-min interval identifier for weekdays and weekends and their daily pattern is plotted.

```r
Avg5MinInt2_weekday <- aggregate(.~interval, data = activity2[activity2$DayType=="weekday",c(1,3)], mean)
Avg5MinInt2_weekend <- aggregate(.~interval, data = activity2[activity2$DayType=="weekend",c(1,3)], mean)

par(mfrow = c(2, 1))
    plot(y=Avg5MinInt2_weekday$steps,x=paste( Avg5MinInt2_weekday$interval),type = "l", xlab = "5-Min Interval Identifier",ylim = c(0,250),ylab = "Average number of Steps")
    title("Daily Step Pattern for Weekdays")
    plot(y=Avg5MinInt2_weekend$steps,x=paste( Avg5MinInt2_weekend$interval),type = "l", xlab = "5-Min Interval Identifier",ylim = c(0,250),ylab = "Average number of Steps")
    title("Daily Step Pattern for Weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 












