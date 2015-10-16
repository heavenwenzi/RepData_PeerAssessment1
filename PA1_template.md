# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```


```r
activityDF <- read.csv("activity.csv")
activityDF$date <- as.Date(activityDF$date)
```

## What is mean total number of steps taken per day?

Here is a histogram of the total number of steps taken each day, with vertical line marking the mean. 


```r
summarizeStepPerDay <- activityDF %>% group_by(date) %>% summarize(total=sum(steps))
meanStepPerDay <- mean(summarizeStepPerDay$total, na.rm=T)
medianStepPerDay <- median.default(summarizeStepPerDay$total, na.rm=T)

dailyMeanHis <- ggplot() +
      geom_histogram(aes(x=total, y=(..count..), fill='r', colour='r'), alpha=0.2, data =summarizeStepPerDay) +
      scale_colour_manual(name="group", values=c("r" = "red"), 
                          labels=c("r"="NA ignored")) +    
      scale_fill_manual(name="group", values=c("r" = "red"), 
                        labels=c("r"="NA ignored")) +
      geom_vline(aes(xintercept=meanStepPerDay), colour='red', linetype="dashed", size=1) +
      ggtitle("histogram of total step per day")+ 
      xlab('total step per day') +
      ylab('count')
print(dailyMeanHis)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

The mean and median total number of steps taken per day are 1.0766189\times 10^{4} and 
10765 respectively. 

## What is the average daily activity pattern?

Let's make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
summarizeStepPerInterval <- activityDF %>% group_by(interval) %>% summarize(average=mean(steps, na.rm=T))
with(summarizeStepPerInterval, plot(x=interval, y=average, type='l',
                                    main="average step of each interval across all days"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
maxStepPerInterval <- max(summarizeStepPerInterval$average)
maxStepInterval <- summarizeStepPerInterval$interval[summarizeStepPerInterval$average==maxStepPerInterval]
```

Time interval 835 has maximum step of 206.1698113. 

## Imputing missing values


```r
naSum <- sum(is.na(activityDF$step))
```

There are 2304 NA entry in the data set. 

Let's fill in the NA values with the average number of steps during that interval to avoid bias. 


```r
newStep <- c()
for (i in 1:length(activityDF$step)) {
  if (is.na(activityDF$step[i])) { 
    newStep[i] <- summarizeStepPerInterval$average[summarizeStepPerInterval$interval==activityDF$interval[i]]
  }
  else {
    newStep[i] <- activityDF$step[i]
  }
}
newDF <- activityDF
newDF$steps <- newStep
```

Again, here is the overlayed histogram of the total number of steps taken each day, with vertical line marking the mean. 


```r
newStepPerDay <- newDF %>% group_by(date) %>% summarize(total=sum(steps))
newMeanStepPerDay <- mean(newStepPerDay$total)
newMedianStepPerDay <- median.default(newStepPerDay$total)

newDailyMeanHis <- ggplot() +
      geom_histogram(aes(x=total, y=(..count..), fill='r', colour='r'), alpha=0.2, data =summarizeStepPerDay) +
      geom_histogram(aes(x=total, y=(..count..), fill='b', colour='b'), alpha=0.2, data =newStepPerDay) +
      scale_colour_manual(name="group", values=c("r" = "red", "b"="blue"), 
                          labels=c("b"="NA filed", "r"="NA ignored")) +    
      scale_fill_manual(name="group", values=c("r" = "red", "b"="blue"), 
                        labels=c("b"="NA filed", "r"="NA ignored")) +
      geom_vline(aes(xintercept=meanStepPerDay), colour='red', linetype="dashed", size=1) +
      geom_vline(aes(xintercept=newMeanStepPerDay), colour='blue', linetype="dashed", size=1) +
      ggtitle("histogram of total step per day")+ 
      xlab('total step per day')+
      ylab('count')
print(newDailyMeanHis)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

The new mean and median total number of steps taken per day are 1.0766189\times 10^{4} and 
1.0766189\times 10^{4} respectively. Compare to previous numbers 1.0766189\times 10^{4} and 
10765, the mean stayed the same but the medium increased and got closer to mean. This is because the NA's that got filled in are all equal to mean. 

## Are there differences in activity patterns between weekdays and weekends?

lets separate the data into weekday and weekend, and make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
# Creating a new variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

## add column that indicates which weekdays they are in integer
newDF$weekdays <- as.POSIXlt(newDF$date)$wday
## newfactor<-as.factor(c('weekend','weekday'))
newDF <- mutate(newDF, day=ifelse((weekdays==0|weekdays==6),'weekend','weekday' ))
newStepPerInterval <- newDF %>% group_by(day,interval) %>% summarize(average=mean(steps, na.rm=T))

# time series plot 
timeSeriesPlot <- ggplot(data=newStepPerInterval, aes(x=interval, y=average)) + 
  geom_line()+
  facet_grid(day~.)+
  ggtitle("average step of each interval across all days")+ 
  xlab('time interval in a day')
print(timeSeriesPlot)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

From the plot, looks like the big spikes are both at earlier of the day, although the steps shifted to later of the day during the weekend comparing to during the weekdays - probably because people sleeps in. Around midday of the weekday, the steps are around 50, during weekend is around 100 - the lower steps probably due to people sitting at the desk at work. 
