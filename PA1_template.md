---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
data = read.csv("activity.csv", header=TRUE, sep=",", na.strings="NA")
data$date = as.Date(as.character(data$date), "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
dataTidy = data[!is.na(data$steps), ]

sumStep = aggregate(steps~date, dataTidy, sum)

hist(sumStep$steps, col="blue", main="number of steps taken per day", xlab="Steps Per Day")
```

![](PA1_template_files/figure-html/part2-1.png)<!-- -->

```r
meanStep = round(mean(sumStep$steps),digits=2)
medianStep = median(sumStep$steps)

print(paste("The mean is: ", meanStep))
```

```
## [1] "The mean is:  10766.19"
```

```r
print(paste("The median is: ", medianStep))
```

```
## [1] "The median is:  10765"
```






## What is the average daily activity pattern?

```r
avgStep = aggregate(steps~interval, dataTidy, mean)

plot(avgStep$interval, avgStep$step, type="l", col="brown", xlab="5-min intervals", ylab="Avarage number of steps taken", main="Average daily activity pattern")
```

![](PA1_template_files/figure-html/part3-1.png)<!-- -->

```r
maxIdx = which.max(avgStep$steps)
maxInterval = avgStep[maxIdx, 'interval']
maxAvgStep = avgStep[maxIdx, 'steps']

print(paste("Interval containing the most steps on average: ", maxInterval))
```

```
## [1] "Interval containing the most steps on average:  835"
```

```r
print(paste("Average steps for that interval: ", round(maxAvgStep, digits=2)))
```

```
## [1] "Average steps for that interval:  206.17"
```


## Imputing missing values

```r
totalNA = sum(is.na(data$steps))

print(paste("The total number of rows with NA is: ", totalNA))
```

```
## [1] "The total number of rows with NA is:  2304"
```

```r
naIdx = which(is.na(data$steps))
naInterval = data[naIdx, 3] 
fillSteps = sapply(naInterval, function(x) { avgStep[(avgStep$interval==x), 2]})

dataNew = data
dataNew[naIdx, 'steps'] = fillSteps 

sumStepNew = aggregate(steps~date, dataNew, sum)

hist(sumStepNew$steps, col="red", main="number of steps taken per day", xlab="Steps Per Day")
```

![](PA1_template_files/figure-html/part4-1.png)<!-- -->

```r
meanStepNew = mean(sumStepNew$steps)
medianStepNew = median(sumStepNew$steps)

print(paste("The mean is: ", round(meanStepNew, digits=2)))
```

```
## [1] "The mean is:  10766.19"
```

```r
print(paste("The median is: ", round(medianStepNew, digits=2)))
```

```
## [1] "The median is:  10766.19"
```


## Are there differences in activity patterns between weekdays and weekends?


```r
dataNew['dateIs'] = factor(sapply(dataNew$date, function(x){ if (weekdays(x) == "Sunday" | weekdays(x) == "Saturday") { "weekend" } else { "weekday"} }))

avgStepDateIs = aggregate(steps~interval + dateIs, mean, data=dataNew)

library(lattice)
xyplot( steps ~ interval | dateIs, data = avgStepDateIs, type="l", layout=c(1,2), xlab="Interval", ylab="Number of steps")
```

![](PA1_template_files/figure-html/part5-1.png)<!-- -->

