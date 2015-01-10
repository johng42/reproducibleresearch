library(ggplot2)
set.seed(75961)#Nacogdoches, TX!!!
actData<-read.csv("C:\\Users\\John\\OneDrive\\Documents\\r_all\\repro research\\activity.csv",na.strings = c("NA",""))
actData <- read.csv(file.choose(), na.strings = c("NA", ""))


head(actData)
summary(actData)
str(actData)

plot(actData)

#first convert the date column to an actual date format
actData$date<-as.Date(actData$date)

sumOfSteps <- with(actData, tapply(steps,date,sum,na.rm=TRUE))
myDataFrame <- data.frame(day=unique(actData$date), sumOfSteps = sumOfSteps)

plot(myDataFrame$day,myDataFrame$sumOfSteps)

histPlot<- qplot(sumOfSteps, data=myDataFrame, geom="histogram",stat='bin',xlab="Number of steps", ylab="Days")
histplot<- histPlot + scale_y_discrete(labels=c(0:10)) #set the y axis better
histplot
mean(sumOfSteps)
median(sumOfSteps)

#now examine the activity pattern
#use same logic as above to create a data frame for this analysis
steps <- with(actData, tapply(steps, as.character(interval), sum, na.rm=TRUE))
myDataFrameInterval <- data.frame(interval=unique(as.character(actData$interval)))
myDataFrameInterval$stepsPerInterval <-  steps
#need average steps of time interval per day
myDataFrameInterval$avgSteps <- steps/length(myDataFrame$day)

#make a time series plot
plot(x=myDataFrameInterval$interval, y=myDataFrameInterval$avgSteps, 
     type='l', xlab='5 minute interval', ylab="Average number of steps")

a<-which.max(myDataFrameInterval$avgSteps)
#convert from base60 (time) to time
hour <- a[[1]]/12   #12 5 minute intervals per hour
mins <- (hour-as.integer(hour))*60 # so hours is now 24 hour clock hours, minutes is minutes
mins <- mins - 5 #because time started at zero at the beginning
print(toString(c("The most steps were taken at",as.integer(hour),":",as.integer(mins))))

#yes, the formatting of the string here is poor :(
      
#count the na values in dataset
naCount <- sum(is.na(actData))
print (toString(c("NA count is " ,naCount)))

#IMPUTE,  fill in the missing NA values with the mean for that 5 minute interval
myFinalDataFrame<-actData
myFinalDataFrame[which(is.na(myFinalDataFrame$steps)),1] <- myDataFrameInterval$avgSteps

sumOfSteps2 <- with(myFinalDataFrame, tapply(steps,date,sum,na.rm=TRUE))
mean(sumOfSteps2)
median(sumOfSteps2)


#generate a histogram of steps per day
histPlot2<- qplot(sumOfSteps2, data=data.frame(day=unique(myFinalDataFrame$date), 
                                              sumOfSteps = sumOfSteps2), geom="histogram",stat='bin',xlab="Number of steps", ylab="Days")
histplot2<- histPlot + scale_y_discrete(labels=c(0:10)) #set the y axis better
histplot2

#weekday/weekend comparison

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
#write.csv(file="myFinalDataFrame.csv",x=myFinalDataFrame)



#plot weekday vs. weekends
a2<-aggregate( steps ~ interval + weekend ,data=myFinalDataFrame, FUN=mean)

ggplot(a2,aes(interval,steps, color='blue'))+
    geom_line()+
    facet_grid(weekend~.) +
    xlab('Interval') +
    ylab('Number of steps')
#oddly, this draws as red on my machine.  Sigh.

