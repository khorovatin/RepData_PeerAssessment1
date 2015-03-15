# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

### 1. Load the data

```r
activity <- read.csv(unz("activity.zip", "activity.csv"), na.strings="NA")
```

### 2. Process/transform the data into a suitable format

```r
activity$date <- as.Date(as.character(activity$date), "%Y-%m-%d")
activity$interval <- as.factor(activity$interval)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```


## What is the mean total number of steps taken per day?
### 1. Calculate the total number of steps taken per day

```r
steps <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
```

### 2. Make a histogram of the total number of steps taken each day

```r
hist(steps, breaks=20, col="lightgreen", xlab="Steps", 
     main="Total Number of Steps Taken Per Day")
rug(jitter(steps, amount=100))
abline(v=mean(steps), col="blue", lwd=2)
abline(v=median(steps), col="green3", lwd=2, lty=2)
legend("topright", c("mean", "median"), col=c("blue", "green3"), 
       lty=c(1, 2), lwd=c(2, 2), bty="n")
```

![](PA1_template_files/figure-html/total_steps_histogram-1.png) 

### 3. Calculate and report the mean and median of total number of steps taken per day

The mean number of steps per day is 9354, and the 
median is 10395.


## What is the average daily activity pattern?
The average daily activity pattern is represented by the mean of the values of 
each interval across all days of the study.

### 1. Make a time series plot

```r
pattern <- tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
plot(pattern, type="l", 
     xlab="Time Interval",
     ylab="Mean Number of Steps per Time Interval",
     main="Average Daily Steps per 5 Minute Interval")
abline(v=which.max(as.vector(pattern)), col="blue", lwd=2)
legend("topright", "maximum", col="blue", lty=1, lwd=2, bty="n")
```

![](PA1_template_files/figure-html/daily_activity_plot-1.png) 

### 2. Which 5-minute interval contains the maximum number of steps, on average
The 104th 5-minute interval, on average across all
days, contains the maximum number of steps, with 
206 steps.

## Imputing missing values
### 1. Calculate and report the total number of missing values
There are a total of 2304 missing values (i.e., coded as `NA`) 
in the dataset. 

### 2. Devise a strategy to fill in the missing values
In order to minimize the effect of this missing data, the 
analysis shall impute missing data by replacing the missing values with the 
median of the step values for the same time interval on days with valid data.
I've adapted the method shown in the forum post at 
https://class.coursera.org/repdata-012/forum/thread?thread_id=40#post-263.

### 3. Create a new dataset containing all the data plus imputed values

```r
# First calculate the median number of steps per interval for all days
interval_median <- aggregate(steps ~ interval, data=activity, median)

# Rename the "steps" column to make the merge and imputation easier
names(interval_median)[2] <- "imputed"

# Merge the activity and inerval median data frames by interval
imputed_activity <- merge(activity, interval_median, by="interval")

# Replace missing steps values with the imputed value
imputed_activity$steps[is.na(imputed_activity$steps)] <- 
        imputed_activity$imputed[is.na(imputed_activity$steps)]

# Remove the column containing the medians, as it is no longer needed
imputed_activity$imputed <- NULL
```

### 4. Create a histogram of the new dataset and calculate mean and median

```r
# Plot, as previously.
imputed_steps <- tapply(imputed_activity$steps, imputed_activity$date, sum)
hist(imputed_steps, breaks=20, col="lightgreen", xlab="Steps", 
     main="Total Number of Steps Taken Per Day",
     sub="(Missing data replaced with median over all days for that interval)")
rug(jitter(imputed_steps, amount=100))
abline(v=mean(imputed_steps), col="blue", lwd=2)
abline(v=median(imputed_steps), col="green3", lwd=2, lty=2)
legend("topright", c("mean", "median"), col=c("blue", "green3"), 
       lty=c(1, 2), lwd=c(2, 2), bty="n")
```

![](PA1_template_files/figure-html/imputed_histogram-1.png) 

The mean number of steps per day with missing data imputed is 
9354, and the median is 10395, compared
with the mean of 9354 and median of10395
from the original data set with missing data. This shows that the mean and median
values were not impacted by this imputation strategy.

## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable indicating whether the data is a weekday or weekend

```r
suppressMessages(library(dplyr))

# Add a column to the activity data to indicate what part of the week the data
# is from (i.e., weekday or weekend)
imputed_activity <- mutate(imputed_activity, 
                   weekpart=as.factor(ifelse(weekdays(imputed_activity$date, 
                                                      abbreviate=TRUE) %in% 
                                                     c("Sat", "Sun"), 
                                             "weekend", "weekday")))
summary(imputed_activity)
```

```
##     interval         steps          date               weekpart    
##  0      :   61   Min.   :  0   Min.   :2012-10-01   weekday:12960  
##  5      :   61   1st Qu.:  0   1st Qu.:2012-10-16   weekend: 4608  
##  10     :   61   Median :  0   Median :2012-10-31                  
##  15     :   61   Mean   : 33   Mean   :2012-10-31                  
##  20     :   61   3rd Qu.:  8   3rd Qu.:2012-11-15                  
##  25     :   61   Max.   :806   Max.   :2012-11-30                  
##  (Other):17202
```

### 2. Make a panel plot of the data averaged across the week parts

```r
suppressMessages(library(lattice))

# Group the data by weekday/weekend and interval
imputed_pattern <- group_by(imputed_activity, weekpart, interval)

# Get the mean accross the groups
imputed_pattern <- summarize(imputed_pattern, steps=mean(steps))

# Create a list of x-axis ticks to use to clean up the graph
at <- seq(0, max(as.numeric(imputed_pattern$interval)), by=50)

# Plot in two panels, one for each week part
xyplot(steps ~ interval|weekpart, data=imputed_pattern, 
       type="l", xlab="Interval", ylab="Number of Steps", 
       main="Activity Patterns on Week Days and Weekends", 
       layout=c(1, 2), scales=list(at=at))
```

![](PA1_template_files/figure-html/weekpart_pattern-1.png) 
