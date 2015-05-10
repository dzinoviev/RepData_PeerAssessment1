# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv ("activity.csv")
activity$date <- as.Date (activity$date)
complete.activity <- activity[complete.cases (activity),]
```


## What is mean total number of steps taken per day?

```r
sum.steps <- aggregate (complete.activity$steps, by = list (complete.activity$date), FUN = sum)[2]
hist (sum.steps$x, xlab = "Daily sum of steps", main = "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean (sum.steps$x)
```

```
## [1] 10766.19
```

```r
sd (sum.steps$x)
```

```
## [1] 4269.18
```

## What is the average daily activity pattern?

```r
steps.by.interval <- aggregate (complete.activity$steps, by = list (complete.activity$interval), 
                                FUN = mean)
with (steps.by.interval, plot (Group.1, x, type = "l", xlab = "Interval", 
                               ylab = "Average number of steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
most.steps <- which.max (steps.by.interval$x)
busy.interval <- steps.by.interval[most.steps, 1]
busy.interval
```

```
## [1] 835
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

```r
dim (activity)[1] - dim (complete.activity)[1]
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
mean.steps <- aggregate (complete.activity$steps, by = list (complete.activity$interval), FUN = mean)
clean.activity <- merge (activity, mean.steps, by.x = "interval", by.y = "Group.1")
clean.activity$steps[is.na (clean.activity$steps)] <- clean.activity$x[is.na (clean.activity$steps)]
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
clean.sum.steps <- aggregate (clean.activity$steps, by = list (clean.activity$date), FUN = sum)[2]
hist (clean.sum.steps$x, xlab = "Daily sum of steps", main = "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
mean (clean.sum.steps$x)
```

```
## [1] 10766.19
```

```r
sd (clean.sum.steps$x)
```

```
## [1] 3974.391
```

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
clean.activity$weekend <- (weekdays (clean.activity$date) == "Sunday" | weekdays (clean.activity$date) == "Saturday")
clean.activity$weekend.factor[clean.activity$weekend] <- "weekend"                   
clean.activity$weekend.factor[!clean.activity$weekend] <- "weekday"
clean.activity$weekend.factor = as.factor (clean.activity$weekend.factor)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library (ggplot2)
clean.steps.by.interval <- aggregate (clean.activity$steps, by = list (clean.activity$interval,clean.activity$weekend.factor), 
                                FUN = mean)
qplot (Group.1, x, data = clean.steps.by.interval, facets = Group.2 ~ ., geom = "line",
       xlab = "Interval", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 
