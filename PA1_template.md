# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv(unz("activity.zip", "activity.csv"), )
data <- transform(data, date = as.Date(data$date, "%Y-%m-%d"))
```

## What is mean total number of steps taken per day?

```r
stepsperday <- c()
for (i in min(data$date):max(data$date)) {
    stepsperday <- c(stepsperday, sum(data[data$date == i, ]$steps))
}
hist(stepsperday, xlab = "Steps taken per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(stepsperday, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(stepsperday, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
meanperinterval <- c()
for (i in unique(data$interval)) {
    meanperinterval <- c(meanperinterval, mean(data[data$interval == i, ]$steps, 
        na.rm = TRUE))
}
plot(unique(data$interval), meanperinterval, type = "l", xlab = "Interval", 
    ylab = "Mean steps per interval")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

## Imputing missing values

```r
miss <- is.na(data$steps)
sum(miss)
```

```
## [1] 2304
```

```r
meanandinterval <- data.frame(interval = unique(data$interval), meanperinterval)
data[miss, ]$steps <- meanandinterval$meanperinterval
stepsperday <- c()
for (i in min(data$date):max(data$date)) {
    stepsperday <- c(stepsperday, sum(data[data$date == i, ]$steps))
}
hist(stepsperday, xlab = "Steps taken per day", main = "AFTER REPLACE")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
mean(stepsperday, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(stepsperday, na.rm = TRUE)
```

```
## [1] 10766
```

## Are there differences in activity patterns between weekdays and weekends?

```r
data <- transform(data, date = factor(weekdays(data$date), levels = c("Monday", 
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
par(mfrow = c(2, 1))
meanperinterval <- c()
for (i in unique(data$interval)) {
    meanperinterval <- c(meanperinterval, mean(data[(data$interval == i & unclass(data$date) <= 
        5), ]$steps, na.rm = TRUE))
}
plot(unique(data$interval), meanperinterval, type = "l", xlab = "Interval", 
    ylab = "Mean steps per interval")
meanperinterval <- c()
for (i in unique(data$interval)) {
    meanperinterval <- c(meanperinterval, mean(data[(data$interval == i & unclass(data$date) >= 
        6), ]$steps, na.rm = TRUE))
}
plot(unique(data$interval), meanperinterval, type = "l", xlab = "Interval", 
    ylab = "Mean steps per interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

