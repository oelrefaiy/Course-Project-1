---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## Loading and preprocessing the data


```r
library(tidyverse)
```

```
## -- Attaching packages ----------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.0     v purrr   0.3.2
## v tibble  2.1.3     v dplyr   0.8.5
## v tidyr   1.0.2     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
```

```
## -- Conflicts -------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
activity<-read.csv("activity.csv")
activity$date<-ymd(activity$date)
```
Some Exploring base of data

```r
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
dim(activity)
```

```
## [1] 17568     3
```

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

```r
library(ggplot2)
library(tidyverse)

subsetData<- activity %>% 
        group_by(date) %>%
        summarise(Total_step=sum(steps,na.rm=TRUE)) %>%
        arrange(desc(Total_step))
head(subsetData)
```

```
## # A tibble: 6 x 2
##   date       Total_step
##   <date>          <int>
## 1 2012-11-23      21194
## 2 2012-11-22      20427
## 3 2012-10-12      17382
## 4 2012-10-06      15420
## 5 2012-10-31      15414
## 6 2012-11-18      15110
```

```r
ggplot(subsetData,aes(x=date,y=Total_step))+
        geom_line() + 
        ylab("Total Steps")+xlab("Date")+
        ggtitle("Total Steps by date")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
png("plot1.png")
#Total Steps by date bar plot
ggplot(subsetData,aes(x=date,y=Total_step))+
        geom_line() + 
        ylab("Total Steps")+xlab("Date")+
        ggtitle("Total Steps by date")
dev.off()
```

```
## png 
##   2
```

```r
#Histogram of total steps
ggplot(subsetData,aes(x=Total_step))+
        geom_histogram()+
        xlab("Total Steps")+ ylab("Counts")+
        ggtitle("Total Steps Historgram")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
png("plot1.1.png")
ggplot(subsetData,aes(x=Total_step))+
        geom_histogram()+
        xlab("Total Steps")+ ylab("Counts")+
        ggtitle("Total Steps Historgram")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```r
dev.off()
```

```
## png 
##   2
```

##Mean and median number of steps taken each day

```r
mean(subsetData$Total_step)
```

```
## [1] 9354.23
```

```r
median(subsetData$Total_step)
```

```
## [1] 10395
```
## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
library(ggplot2)
AvgDailyActivity<- activity %>% group_by(interval) %>%
        summarise(Avg_Steps=mean(steps ,na.rm = T))

ggplot(AvgDailyActivity,aes(x=interval,y=Avg_Steps))+
        geom_line()+
        ylab("Mean Steps Every interval")+
        xlab("Interval")+ggtitle("Mean Steps by Interval daily")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
png("plot2.png")
ggplot(AvgDailyActivity,aes(x=interval,y=Avg_Steps))+
        geom_line()+
        ylab("Mean Steps Every interval")+
        xlab("Interval")+ggtitle("Mean Steps by Interval daily")
        
dev.off()
```

```
## png 
##   2
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
AvgDailyActivity[which.max(AvgDailyActivity$Avg_Steps), ]$interval
```

```
## [1] 835
```


## Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


```r
library(ggplot2)
#sum missing values
sums<-sum(is.na(activity$steps))
prop<-sum(is.na(activity$steps))/dim(activity)[[1]]
```
   we can see that sumMissing is 2304 and proportation is 0.1311475
   

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
library(tidyverse)
#replace Na with mean of Steps
activity$steps[is.na(activity$steps)]<-AvgDailyActivity$Avg_Steps
head(activity)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
library(dplyr)
SumSteps<-activity %>% group_by(date) %>%
        summarise(dialy_steps=sum(steps))%>%
        arrange(desc(dialy_steps))
head(SumSteps)
```

```
## # A tibble: 6 x 2
##   date       dialy_steps
##   <date>           <dbl>
## 1 2012-11-23       21194
## 2 2012-11-22       20427
## 3 2012-10-12       17382
## 4 2012-10-06       15420
## 5 2012-10-31       15414
## 6 2012-11-18       15110
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
library(ggplot2)
ggplot(SumSteps ,aes(x=dialy_steps))+
        geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
png("plot3.png")
ggplot(SumSteps ,aes(x=dialy_steps))+
        geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```r
dev.off()
```

```
## png 
##   2
```
the mean and median of the total number of steps taken per day

```r
mean(SumSteps$dialy_steps)
```

```
## [1] 10766.19
```

```r
median(SumSteps$dialy_steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
library(lubridate)
activity$weekday<-wday(activity$date,label = T)
levels(activity$weekday)<-c(1,2,3,4,5,6,7)
activity$datetype<-activity$weekday %in% c(1,2,3,4,5)
FinalData<-activity %>%
        group_by(interval,datetype) %>%
        summarise(MeanSteps=mean(steps, na.rm = T))
FinalData$datetype<-as.factor(FinalData$datetype)
levels(FinalData$datetype)<-c("Weekend","Weekday")
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data

```r
library(ggplot2)
ggplot(FinalData, aes(x = interval , y = MeanSteps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by type of date", 
            x = "Interval", y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
png("plot4.png")
ggplot(FinalData, aes(x = interval , y = MeanSteps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by type of date", 
            x = "Interval", y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
dev.off()
```

```
## png 
##   2
```



