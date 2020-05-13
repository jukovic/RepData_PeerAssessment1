Reproducible Research Assignment 1
=============================================


Loading and preprocessing the data

```r
urlzip <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(urlzip, destfile = "./activitymonitoringzip")
unzip("activity_data.zip", exdir = "./activitymonitoring")
activity<-read.csv("activity.csv",header=TRUE,na.strings="NA")
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

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
Convert date to date format

```r
activity$date<-as.Date(activity$date)
```
Histogram of the total number of steps taken per day

```r
library(ggplot2)
aggpd <- aggregate(steps ~ date, FUN = sum, data = activity)
hist(aggpd$steps)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)









Mean number of steps taken per day 

```r
aggmean <- mean(aggpd$steps)
aggmean
```

```
## [1] 10766.19
```
Median number of steps taken per day

```r
aggmedian <- median(aggpd$steps)
aggmedian
```

```
## [1] 10765
```
Daily average activity pattern

```r
fiveminuteav <- aggregate(steps~interval, data=activity, FUN=mean, na.rm=TRUE)
plot(x = fiveminuteav$interval, y = fiveminuteav$steps, type = "l") 
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)










Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

```r
max_steps <- max(fiveminuteav$steps)
for (i in 1:288) 
{
    if (fiveminuteav$steps[i] == max_steps)
        five_minute_interval_at_max_steps <- fiveminuteav$interval[i]
}
five_minute_interval_at_max_steps
```

```
## [1] 835
```
Total number of missing values

```r
sum(is.na(activity))
```

```
## [1] 2304
```
Missing value imputation strategy

```r
impute.activity <- activity
impute.activity$steps[is.na(impute.activity$steps)] <- mean(impute.activity$steps,na.rm=TRUE)
impute.activity$steps <- as.numeric(impute.activity$steps)
impute.activity$interval <- as.numeric(impute.activity$interval)
```
New dataset that is equal to the original dataset but with the missing data filled in

```r
summary(impute.activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 37.38   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```
Make a histogram of the total number of steps taken each day

```r
aggpdnoNA <- aggregate(steps ~ date, FUN = sum, data = impute.activity)
hist(aggpdnoNA$steps)
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png)








Mean total number of steps taken per day (after impute)

```r
aggmeannoNA<-mean(aggpdnoNA$steps)
aggmeannoNA
```

```
## [1] 10766.19
```
Median total number of steps taken per day (after impute)

```r
aggmediannoNA<-median(aggpdnoNA$steps)
aggmediannoNA
```

```
## [1] 10766.19
```
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day

```r
impute.activity$day <- ifelse(weekdays(impute.activity$date) %in% c("Saturday","Sunday"), "weekday", "weekend")
```
Panel plot time series by day

```r
library(dplyr)
impute.day <- impute.activity %>% group_by(interval,day) %>% summarise(mean.step=mean(steps))
weekdayinterval <- ggplot(impute.day, aes(x=interval, y=mean.step, color=day)) + 
  facet_grid(day~.) +
  geom_line() + 
  labs(title="Average Number of Steps Taken vs 5-min Interval on Weekday/Weekend", y="Average Number of Steps", x="5 minute Interval Times Series")
weekdayinterval
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28-1.png)
