# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv(unz("activity.zip", "activity.csv"), na.strings = "NA")
data$date <- as.Date(as.character(data$date), "%Y-%m-%d")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?

```r
steps <- tapply(data$steps, data$date, sum, na.rm = T)
hist(steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(steps)
```

```
## [1] 9354.23
```

```r
median(steps)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

```r
pattern <- tapply(data$steps, data$interval, mean, na.rm = T)
plot(pattern, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
pattern[which.max(pattern)]
```

```
##      835 
## 206.1698
```


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
