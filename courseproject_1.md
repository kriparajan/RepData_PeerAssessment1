# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data



```r
library(ggplot2)
library(plyr)
library(lattice)
setwd( "C:/Users/Kripa/Desktop/R programming Directory")
activity <- read.csv("activity.csv")

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
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

### Calculate the total number of steps taken per day

```r
cleanactivity<- activity[!is.na(activity$steps),]
sumtable <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(sumtable)<- c("Date", "Steps")
```

### Make a histogram of the total number of steps taken each day

```r
hist(sumtable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day", col="red")
```

![](courseproject_1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Calculate and report the mean and median of the total number of steps taken per day

```r
mean(sumtable$Steps)
```

```
## [1] 10766.19
```

```r
median(sumtable$Steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
### and the average number of steps taken, averaged across all days (y-axis)

```r
cleanactivity <- activity[!is.na(activity$steps),]
intervaltable <- ddply(cleanactivity, .(interval), summarize, Avg = mean(steps))


plotline <- ggplot(intervaltable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
plotline + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
```

![](courseproject_1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### 5-minute interval that contains the maximum steps 


```r
maxSteps <- max(intervaltable$Avg)
intervaltable[intervaltable$Avg==maxSteps,1]
```

```
## [1] 835
```

## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs

### Number of NAs in original data set

```r
nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```
### Create the average number of steps per weekday and interval

```r
avgtable <- ddply(cleanactivity, .(interval, day), summarize, Avg = mean(steps))
```

### Create dataset with all NAs for substitution and merge the NA data 

```r
nadata<- activity[is.na(activity$steps),]
newdata<-merge(nadata, avgtable, by=c("interval", "day"))
newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")
```

### Merge the NA averages and non NA data together


```r
datamerge <- rbind(cleanactivity, newdata2)
sumtable2 <- aggregate(datamerge$steps ~ datamerge$date, FUN=sum, )
colnames(sumtable2)<- c("Date", "Steps")
```

### find mean and median of this new dataset

```r
mean(sumtable2$Steps)
```

```
## [1] 10821.21
```

```r
median(sumtable2$Steps)
```

```
## [1] 11015
```
### Create histogram of total steps per day 

```r
hist(sumtable2$Steps, breaks=6, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="red")
hist(sumtable$Steps, breaks=6, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="blue", add=T)
legend("topright", c("Data", "Non-NA Data"), fill=c("red", "blue") )
```

![](courseproject_1_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?

### create new category based on weekday vs weekend

```r
datamerge$DayCategory <- ifelse(datamerge$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```

### Summarize data by interval and the type of day

```r
intervaltable2 <- ddply(datamerge, .(interval, DayCategory), summarize, Avg = mean(steps))
```

### Plot this data for weekday and weekend 

```r
xyplot(Avg~interval|DayCategory, data=intervaltable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
```

![](courseproject_1_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
