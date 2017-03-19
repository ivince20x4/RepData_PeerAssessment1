# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1. Load the data


```r
activity=read.csv("activity.csv")
```


2. Process the data for analysis


```r
totalSteps<-aggregate(steps~date,data=activity,sum,na.rm=TRUE)
```


## What is mean total number of steps taken per day?

1. You have to create a histogram of the total no. of steps to be taken each day/


```r
hist(totalSteps$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

2. Now, get the value of the mean and median of the total no. of steps to be taken each day. 


```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```

mean is 1.0766189\times 10^{4}
median is 10765

## What is the average daily activity pattern?

1. Create a time series plot of the 5-minute interval and average number of steps taken for all days. 


```r
stepsInterval<-aggregate(steps~interval,data=activity,mean,na.rm=TRUE)
plot(steps~interval,data=stepsInterval,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps


```r
stepsInterval[which.max(stepsInterval$steps),]$interval
```

```
## [1] 835
```

- Most steps are at the 835th

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset 


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```


2. Devise a strategy for filling in all of the missing values in the dataset.

- fill in all missing values with mean for 5-minute interval by having function "interval2steps"

```r
interval2steps<-function(interval){
    stepsInterval[stepsInterval$interval==interval,]$steps
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.



```r
activityFilled<-activity   # Make a new dataset
count=0           # Count no of data to be filled in
for(i in 1:nrow(activityFilled)){
    if(is.na(activityFilled[i,]$steps)){
        activityFilled[i,]$steps<-interval2steps(activityFilled[i,]$interval)
        count=count+1
    }
}
cat("Total ",count, "NA values were filled.\n\r") 
```

```
## Total  2304 NA values were filled.
## 
```


4. Make a histogram of the total number of steps taken each day


```r
totalSteps2<-aggregate(steps~date,data=activityFilled,sum)
hist(totalSteps2$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


5. Calculate and report the mean and median total number of steps taken per day.


```r
mean(totalSteps2$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps2$steps)
```

```
## [1] 10766.19
```
- mean steps: 1.0766189\times 10^{4}
- median steps: 1.0766189\times 10^{4}

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activityFilled$day=ifelse(as.POSIXlt(as.Date(activityFilled$date))$wday%%6==0,
                          "weekend","weekday")
# For Sunday and Saturday : weekend, Other days : weekday 
activityFilled$day=factor(activityFilled$day,levels=c("weekday","weekend"))
```


2. Make a panel plot containing a time series plot


```r
stepsInterval2=aggregate(steps~interval+day,activityFilled,mean)
library(lattice)
xyplot(steps~interval|factor(day),data=stepsInterval2,aspect=1/2,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

