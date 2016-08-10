# 𝙿𝙰𝟷_𝚝𝚎𝚖𝚙𝚕𝚊𝚝𝚎
LB  
7 august 2016  

## Loading and preprocessing data


```r
activity_data <- read.csv("activity.csv")
```

### Omitting na rows

```r
activity_no_NA <- na.omit(activity_data)
```

## Steps per day

### Aggregating steps on for days

```r
sinad <- aggregate(steps ~ date, activity_no_NA, sum) 
```

### Drawing a histogram of the number of steps in a day

```r
hist(sinad[,2], breaks=26, col="green", main="Histogram of steps in a day", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Summarizing the steps per day data

```r
summary(sinad[,2])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

## Average daily pattern

### Aggregating data on interval


```r
sinint_sum <- aggregate(steps ~ interval, activity_no_NA, sum) 
sinint_mean <- aggregate(steps ~ interval, activity_no_NA, mean) 
```
### Plotting the activity

```r
plot(sinint_mean, type='l', col="blue", main="Activity in time interval in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### Finding the interval with the max value

```r
which.max(sinint_mean[,2])
```

```
## [1] 104
```

```r
sinint_mean[104,]
```

```
##     interval    steps
## 104      835 206.1698
```

## Dealing with missing values

### Counting the number

Use the boolean function is.na, and sum the 1-s to get the number

1. Create a function to look up interval averages depending on the recorded time
2. Mutate activity_data to include the above information in NA cases


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
myf<-function(x){y<-sinint_mean[which(sinint_mean[,1]==x),2];   return (y)}
intervals <- sapply(activity_data[,3], myf)
activity_data %>% mutate(steps = ifelse(is.na(steps), intervals, steps)) -> activity_data_augment
```

### Making a histogram of the total number of steps taken

```r
augsinad <- aggregate(steps ~ date, activity_data_augment, sum) 
hist(augsinad[,2], breaks=26, col="cyan", main="Histogram of steps in a day with augmented values", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
summary(augsinad)
```

```
##          date        steps      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10766  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```

## Weekdays and weekends

### Calculate new aggegrate data on the intervals and put in new factor variable

```r
activity_data_augment %>% mutate(daytype = ifelse( weekdays(as.Date(date))<"Fri" , "weekend", "weekday")) -> activity_data_augment
activity_data_augment %>% mutate(daytype = ifelse(weekdays(as.Date(date))<"Mon" , "weekend", "weekday")) -> activity_data_augment
activity_data_augment$daytype <- as.factor(activity_data_augment$daytype)

augsininint_mean <- aggregate(steps ~ interval + daytype, activity_data_augment, mean) 
```

### Plotting the steps against the interval means, for the factor daytype

```r
library(ggplot2)
g <- ggplot(augsininint_mean, aes(interval, steps)) + geom_line() + facet_grid(daytype ~ .)
g
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->






