---
---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data


```{r}
library(ggplot2)
library(plyr)
library(lattice)
setwd( "C:/Users/Kripa/Desktop/R programming Directory")
activity <- read.csv("activity.csv")

head(activity)
dim(activity)
summary(activity)


activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?


```{r}

cleanactivity<- activity[!is.na(activity$steps),]
sumtable <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(sumtable)<- c("Date", "Steps")
hist(sumtable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day", col="red")
mean(sumtable$Steps)
median(sumtable$Steps)

```

## What is the average daily activity pattern?
```{r}

cleanactivity <- activity[!is.na(activity$steps),]
intervaltable <- ddply(cleanactivity, .(interval), summarize, Avg = mean(steps))


plotline <- ggplot(intervaltable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
plotline + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")


maxSteps <- max(intervaltable$Avg)
intervaltable[intervaltable$Avg==maxSteps,1]

```

## Imputing missing values
```{r}

nrow(activity[is.na(activity$steps),])


avgtable <- ddply(cleanactivity, .(interval, day), summarize, Avg = mean(steps))


nadata<- activity[is.na(activity$steps),]
newdata<-merge(nadata, avgtable, by=c("interval", "day"))
newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")
datamerge <- rbind(cleanactivity, newdata2)
sumtable2 <- aggregate(datamerge$steps ~ datamerge$date, FUN=sum, )
colnames(sumtable2)<- c("Date", "Steps")


mean(sumtable2$Steps)
median(sumtable2$Steps)

hist(sumtable2$Steps, breaks=6, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="red")
hist(sumtable$Steps, breaks=6, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="blue", add=T)
legend("topright", c("Data", "Non-NA Data"), fill=c("red", "blue") )

```


## Are there differences in activity patterns between weekdays and weekends?
```{r}

datamerge$DayCategory <- ifelse(datamerge$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")


intervaltable2 <- ddply(datamerge, .(interval, DayCategory), summarize, Avg = mean(steps))

xyplot(Avg~interval|DayCategory, data=intervaltable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")


```