---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
setwd('/Users/knicoter/RepData_PeerAssessment1')
library(data.table)
dt<-data.table(read.csv("./activity.csv"))
```

## What is mean total number of steps taken per day?


```r
#histogram of the total number of steps taken per day
step_sum<-dt[,sum(steps,na.rm=TRUE),by=date]
setnames(step_sum, 'V1', 'date_sum')
hist(step_sum$date_sum, main="Total Taken Steps Per Day", xlab="Steps", ylab="Count")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 


```r
#mean number of steps per day
mean(step_sum$date_sum, na.rm=TRUE)
```

```
## [1] 9354.23
```


```r
#median number of steps per day
median(step_sum$date_sum, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
#time series plot of the 5-minute interval (x-axis) and the 
#average number of steps taken, averaged across all days (y-axis)
avg_int<-dt[,mean(steps,na.rm=TRUE),by=interval]
setnames(avg_int, "V1", "interval_mean")
plot(avg_int, type='l', main="Average Daily Activity Pattern", xlab="Interval", ylab="Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


```r
#Which 5-min time interval contains the maximum number of steps?
max_int<-avg_int[which.max(avg_int$interval_mean)]
max_int$interval
```

```
## [1] 835
```

## Imputing missing values


```r
#total number of missing values in the dataset
na_step<-subset(dt, is.na(steps))
sum(is.na(na_step))
```

```
## [1] 2304
```


```r
#fill in all of the missing values in the dataset
dt2<-subset(merge(na_step, avg_int, by="interval"), TRUE,select=c(interval_mean, date, interval))
setnames(dt2, "interval_mean", "steps")
dt_clean<-rbind(subset(dt, !is.na(steps)), dt2)
```


```r
#create a new dataset that is equal to the original dataset 
#but with the missing data filled in
step_sum_clean<-dt_clean[,sum(steps,na.rm=TRUE),by=date]
setnames(step_sum_clean, "V1", "date_sum")
```


```r
#make a histogram of the total number of steps taken each day
hist(step_sum_clean$date_sum, main="Total Steps Per Day", xlab="Total Steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 


```r
#mean number of steps per day
mean(step_sum_clean$date_sum, na.rm=TRUE)
```

```
## [1] 10766.19
```


```r
#median number of steps per day
median(step_sum_clean$date_sum, na.rm=TRUE)
```

```
## [1] 10766.19
```

**Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

Yes. A larger mean change is the result of imputing missing data on the estimates of the total number of steps per day.

## Are there differences in activity patterns between weekdays and weekends?


```r
#factor variable with two levels indicating a weekday or weekend
Sys.setlocale("LC_TIME", "en_US.UTF-8")
dt_clean[,w:=factor(ifelse(weekdays(as.Date(date),abbreviate=TRUE) %in% c("Sat","Sun"), "weekend","weekday"))]
```


```r
library(reshape2)
stepsmelt<-melt(dt_clean,id.vars=c("w","interval"), measure="steps")
steps_mean<-dcast(stepsmelt, w+interval~variable, fun=mean)

library(lattice)
xyplot(steps ~ interval | w, data = steps_mean,type='l', layout = c(1, 2), ylab="Number of Steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 
