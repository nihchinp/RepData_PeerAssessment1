---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

```r
setwd('C:/Users/Lenovo/Documents/Coursera/Reproducible Research/Week 2')
activity <- read.csv("activity.csv")
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
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

## What is mean total number of steps taken per day?
## (1) Calculate the total number of steps taken per day
## (2) Histogram of total number of steps taken per day

```r
steps_day <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
hist(steps_day$steps, xlab = "Steps each day", main = "Total number of steps taken each day", col = "blue")
```

![](Project_1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## (3) Mean and median of the total number of steps taken per day

```r
mean_steps <- mean(steps_day$steps)
mean_steps <- round(mean_steps, digits = 1)
print(mean_steps)
```

```
## [1] 10766.2
```

```r
median_steps <- median(steps_day$steps)
median_steps <- round(median_steps, digits = 1)
print(median_steps)
```

```
## [1] 10765
```
Answer: The mean step is 1.07662\times 10^{4} and median step is 1.0765\times 10^{4}.

## What is the average daily activity pattern?
## (1) Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
mean_steps_ts <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
plot(mean_steps_ts$interval, mean_steps_ts$steps, type = "l", col = "blue", xlab = "Intervals", ylab = "Total steps per interval", main = "Number of steps per interval (averaged) (Exclude NA)")
```

![](Project_1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## (2) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_steps <-max(mean_steps_ts$steps)
max_steps_interval <- mean_steps_ts$interval[which(mean_steps_ts$steps == max_steps)]
print(max_steps_interval)
```

```
## [1] 835
```

```r
max_steps <- round(max_steps, digits = 1)
print(max_steps)
```

```
## [1] 206.2
```
Answer: The maximum steps taken is 206.2 and occurred at interval 835.

## Imputing missing values
## (1) Calculate and report the total number of missing values in the dataset 
## (2) Devise a strategy for filling in all of the missing values in the dataset.
## (3) Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
total_na <- sum(is.na(activity))
Steps_Interval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
activity_split <- split(activity, activity$interval)
#Impute missing data for each interval
for(i in 1:length(activity_split)){
  activity_split[[i]]$steps[is.na(activity_split[[i]]$steps)] <- Steps_Interval[i]
}
activity_imputed <- do.call("rbind", activity_split)
activity_imputed <- activity_imputed[order(activity_imputed$date) ,]
```
Answer: The total number of missing value is 2304.

## (4) Make a histogram of the total number of steps taken per day and calculate the mean/median total number of steps taken per day.

```r
steps_imputed <- tapply(activity_imputed$steps, activity_imputed$date, sum)
hist(steps_imputed, xlab = "Number of Steps", main = "Number of steps per Day (After Imputed)", col = "blue")
```

![](Project_1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
#Mean and median number of steps taken each day
imputed_mean_steps <- mean(steps_imputed)
imputed_mean_steps <- round(imputed_mean_steps, digits = 1)
imputed_median_steps <- median(steps_imputed)
imputed_median_steps <- round(imputed_median_steps, digits = 1)
#Compare the results in a dataframe
compare_mean_median <- data.frame(c(mean_steps, median_steps), c(imputed_mean_steps, imputed_median_steps))
colnames(compare_mean_median) <- c("Before Impute NA", "After impute NA")
rownames(compare_mean_median) <- c("mean", "median")
library(xtable)
xt <- xtable(compare_mean_median)
print(xt, type  = "html")
```

```
## <!-- html table generated in R 3.5.2 by xtable 1.8-3 package -->
## <!-- Mon May 06 08:55:35 2019 -->
## <table border=1>
## <tr> <th>  </th> <th> Before Impute NA </th> <th> After impute NA </th>  </tr>
##   <tr> <td align="right"> mean </td> <td align="right"> 10766.20 </td> <td align="right"> 10766.20 </td> </tr>
##   <tr> <td align="right"> median </td> <td align="right"> 10765.00 </td> <td align="right"> 10766.20 </td> </tr>
##    </table>
```
Answer: Imputing missing values does not change the mean value (1.07662\times 10^{4}) and insignificantly increased the median value (from 1.0765\times 10^{4} to 1.07662\times 10^{4}).

## Are there differences in activity patterns between weekdays and weekends?
## (1) Create a new factor variable in the dataset with two levels - "weekday" and "weekend".
## (2) Make a panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.

```r
activity_imputed$wktype <- ifelse(weekdays(as.Date(activity_imputed$date)) == "Saturday" | weekdays(as.Date(activity_imputed$date)) == "Sunday", "weekend", "weekday")
#Average steps per interval for weekdays and weekends
steps_weekday <- tapply(activity_imputed[activity_imputed$wktype == "weekday" ,]$steps, activity_imputed[activity_imputed$wktype == "weekday" ,]$interval, mean, na.rm = TRUE)
steps_weekend <- tapply(activity_imputed[activity_imputed$wktype == "weekend" ,]$steps, activity_imputed[activity_imputed$wktype == "weekend" ,]$interval, mean, na.rm = TRUE)
#Create a 2 panel plot for weekday and weekend
par(mfrow=c(1,2))
plot(as.numeric(names(steps_weekday)), 
     steps_weekday, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Activity in Weekdays", 
     type = "l")
plot(as.numeric(names(steps_weekend)), 
     steps_weekend, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Activity in Weekends", 
     type = "l")
```

![](Project_1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Answer: Activity (by number of steps) starts earlier during weekdays over a shorter duration (by occurrences of high peaks) while activity starts later during weekends and over a longer duration.
