Coursera Reproducible Research, Assignment 1
========================================================
## Nadia Kudryavtseva

Loading and preprocessing the data:

```r
fileUrl <- "http://d396qusza40orc.cloudfront.net/repdata/data/activity.zip"
download.file(fileUrl, "activity.zip")
data <- read.csv(unz("activity.zip", "activity.csv"))
head(data)
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
Histogram of the total number of steps taken each day:

```r
steps_per_day <- tapply(data$steps, INDEX=data$date, FUN=sum, na.RM=TRUE)
hist(steps_per_day, main="Histogram of total number of steps per day", xlab="Total number of steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(steps_per_day, na.rm=TRUE)
```

```
## [1] 10767
```

```r
median(steps_per_day, na.rm=TRUE)
```

```
## [1] 10766
```

Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days:

```r
day_average_steps <- tapply(data$steps, INDEX=data$interval, FUN=mean, na.rm=TRUE)
intervals <- levels(factor(data$interval))
plot(day_average_steps~intervals, type="l", ylab="Steps (day average)", xlab="Number of 5-minute intervals")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

The 5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps

```r
as.integer(which.max(day_average_steps))
```

```
## [1] 104
```

Calculate total number of missing values in the dataset

```r
na_tot <- 0
for (i in names(data)){
   na_tot <- na_tot + length(data[[i]]) - length(na.omit(data[[i]]))
   print(length(data[[i]]) - length(na.omit(data[[i]])))
}
```

```
## [1] 2304
## [1] 0
## [1] 0
```

```r
na_tot
```

```
## [1] 2304
```

Fill in the missing data with the average value

```r
for (i in 1:length(data$steps)){
   if (is.na(data$steps[i])==TRUE){
      index <- which(data$interval[i] == intervals)[[1]]
      data$steps[i] <- day_average_steps[index]
   }
}
```
Create a histogram of the total number of steps in order to compare with previous results

```r
steps_per_day_new <- tapply(data$steps, INDEX=data$date, FUN=sum, na.RM=TRUE)
hist(steps_per_day_new, main="Histogram of total number of steps per day", xlab="Total number of steps per day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r
mean(steps_per_day_new, na.rm=TRUE)
```

```
## [1] 10767
```

```r
median(steps_per_day_new, na.rm=TRUE)
```

```
## [1] 10767
```
It is seen from the plot and printed medium and mean values, that filling in the data changes:  
1. Frequencies of the distribution  
2. Median

However, is does not change the mean value and the shape of the distribution.


```r
data$dweek <- weekdays(as.Date(data$date))
for (i in 1:length(data$dweek)){
   if ((data$dweek[i]=="Saturday") | (data$dweek[i]=="Sunday")){
      data$dweek[i] <- "Weekend"
   }
   else{
      data$dweek[i] <- "Weekday"
      
   }
}
```

Time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekdays and weekends:

```r
data_weekday <- subset(data, data$dweek=="Weekday")
weekday_average_steps <- tapply(data_weekday$steps, INDEX=data_weekday$interval, FUN=mean, na.rm=TRUE)
data_weekend <- subset(data, data$dweek=="Weekend")
weekend_average_steps <- tapply(data_weekend$steps, INDEX=data_weekend$interval, FUN=mean, na.rm=TRUE)
intervals1 <- levels(factor(data_weekday$interval))
intervals2 <- levels(factor(data_weekend$interval))
par(mfrow=c(2,1))
plot(weekday_average_steps~intervals1, type="l", ylab="Number of steps", xlab="Interval", main="Weekday")
plot(weekend_average_steps~intervals2, type="l", ylab="Number of steps", xlab="Interval", main="Weekend")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 
