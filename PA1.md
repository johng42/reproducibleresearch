# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library("ggplot2")
set.seed(75961)#Nacogdoches, TX!!!
actData <- read.csv(file.choose(), na.strings = c("NA", ""))

head(actData)
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
summary(actData)
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
str(actData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
plot(actData)
```

![](./PA1_files/figure-html/unnamed-chunk-1-1.png) 

There is some relationship between steps and date.

To process the data, I create a new data frame with 2 variables - the date and the sum of the steps taken on that date.


```r
#first convert the date column to an actual date format
actData$date<-as.Date(actData$date)

sumOfSteps <- with(actData, tapply(steps,date,sum,na.rm=TRUE))
myDataFrame <- data.frame(day=unique(actData$date), sumOfSteps = sumOfSteps)
```

## What is mean total number of steps taken per day?

The mean total steps taken per day is:

```r
mean(sumOfSteps)
```

```
## [1] 9354.23
```

The median is:

```r
median(sumOfSteps)
```

```
## [1] 10395
```

And here is a histogram of this activity.


```r
histPlot<- qplot(sumOfSteps, data=myDataFrame, geom="histogram",stat='bin',xlab="Number of steps", ylab="Days")
histplot<- histPlot + scale_y_discrete(labels=c(0:10)) #set the y axis better
histplot
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](./PA1_files/figure-html/unnamed-chunk-5-1.png) 

## What is the average daily activity pattern?
First examine a time series plot.  Use the same logic as above to create a data frame with average steps per time interval to use for plotting, then create the plot.


```r
steps <- with(actData, tapply(steps, as.character(interval), sum, na.rm=TRUE))
myDataFrameInterval <- data.frame(interval=unique(as.character(actData$interval)))
myDataFrameInterval$stepsPerInterval <-  steps
#need average steps of time interval per day
myDataFrameInterval$avgSteps <- steps/length(myDataFrame$day)

#make a time series plot
plot(x=myDataFrameInterval$interval, y=myDataFrameInterval$avgSteps, 
     type='l', xlab='5 minute interval', ylab="Average number of steps")
```

![](./PA1_files/figure-html/unnamed-chunk-6-1.png) 

The 5 minute interval which has the most average steps is index 272, which corresponds to time 22:35.

```r
a<-which.max(myDataFrameInterval$avgSteps)
#convert from base60 (time) to time
hour <- a[[1]]/12   #12 5 minute intervals per hour
mins <- (hour-as.integer(hour))*60 # so hours is now 24 hour clock hours, minutes is minutes
mins <- mins - 5 #because time started at zero at the beginning
print(toString(c("The most steps were taken at",as.integer(hour),":",as.integer(mins))))
```

```
## [1] "The most steps were taken at, 22, :, 35"
```

## Imputing missing values

Now get an idea of how many NA (missing) values there are in the activity data.


```r
naCount <- sum(is.na(actData))
print (naCount)
```

```
## [1] 2304
```
There are 2304 NA values.  Next, I chose to replace the NA values with the average number of steps for that particular 5 minute interval in a new data frame.
Also, a quick check to ensure the previous mean and median did not change.


```r
myFinalDataFrame<-actData
myFinalDataFrame[which(is.na(myFinalDataFrame$steps)),1] <- myDataFrameInterval$avgSteps
sumOfSteps2 <- with(myFinalDataFrame, tapply(steps,date,sum,na.rm=TRUE))
mean(sumOfSteps2)
```

```
## [1] 10581.01
```

```r
median(sumOfSteps2)
```

```
## [1] 10395
```

The asssignment calls for a histogram of this modified data as well:


```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](./PA1_files/figure-html/unnamed-chunk-10-1.png) 

## Are there differences in activity patterns between weekdays and weekends?

To answer this question, I need a function to separate weekdays from weekends based on the date.  Also, update the data frame with this classification.


```r
day <- function(date){
    if (weekdays(date) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
#created a weekend column in data frame
myFinalDataFrame$date<- as.Date(myFinalDataFrame$date)
myFinalDataFrame$weekend <- sapply(myFinalDataFrame$date,day)
```

Create the data for the 2 needed comparison graphs.  This is needed to generate the final dual part graph to fit the example from the class repository.


```r
a2<-aggregate( steps ~ interval + weekend ,data=myFinalDataFrame, FUN=mean)

ggplot(a2,aes(interval,steps, color='blue'))+
    geom_line()+
    facet_grid(weekend~.) +
    xlab('Interval') +
    ylab('Number of steps')
```

![](./PA1_files/figure-html/unnamed-chunk-12-1.png) 

```r
#oddly, this draws as red on my machine.  Sigh.
```

a2 is just a temporary variable to hold the data for graphing.  It is only to keep the code from being too messy to read.  Anyway, there is a difference.  It looks like the volume of steps taken each day starts later in the morning on weekends (about 6AM on weekdays and 8AM on weekends).  People also tend to be more active during the day during weekends compared to weekdays.























