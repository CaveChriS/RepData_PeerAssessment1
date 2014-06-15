Reproducible Reseach - Peer Assignment 1
========================================================

This is an R Markdown document. Markdown is a simple formatting syntax for authoring web pages (click the **Help** toolbar button for more details on using R Markdown).

When you click the **Knit HTML** button a web page will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

The first steps are not code at all! For this to work you need to have the downloaded the data from the coursera website (at your own risk) and then put it into your coursera working directory. Then...extract it into a directory of the same name. Then this code should work

###So lets first set the global options


```r
opts_chunk$set(echo=TRUE)
if(!require(reshape2)) {install.packages("reshape2")}
```

```
## Loading required package: reshape2
```

```r
library(reshape2)
if(!require(lattice)) {install.packages("lattice")}
```

```
## Loading required package: lattice
```

```r
library(lattice)
```

### Then lets read in the file as we are asked to


```r
fileLocation <- paste(getwd(),"/repdata_data_activity/","activity.csv",sep="")
activityFile <- read.csv(fileLocation)
```


Now we have a file lets histogram the steps (leaving the NAs in as asked)


```r
hist(activityFile$steps)
```

![plot of chunk createHistogramOfSteps](figure/createHistogramOfSteps.png) 

Now we need to print out the median and mean but I think we want to do it in a sentence neatly so...


```r
stepsSummary <- summary(activityFile$steps)
medianOfSteps <- stepsSummary[["Median"]]
meanOfSteps <- stepsSummary[["Mean"]]
```

The median of the steps data is 0 and the mean is 37.4.

### plotting interval against average steps

First the calculations where I will hide the results


```r
noNAs <- activityFile[!is.na(activityFile$steps),]
runAmelt <- melt(noNAs,id=c("interval"),measure.vars="steps")
averageData <- dcast(runAmelt,interval ~ variable,mean)
```

Now I need to plot those results


```r
plot(averageData$interval,averageData$steps,type="l",xlab="Interval during day", ylab="No. of steps averaged across all days",main="Average steps against interval")
```

![plot of chunk plotOfStepsAndInterval](figure/plotOfStepsAndInterval.png) 

Now, which interval has the peak. I'll save the result and output is as statement again


```r
higherInterval <- averageData[averageData$steps == max(averageData$steps),]$interval
```

The interval with the highest number of steps is 835.


```r
noOfStepNAs <- sum(is.na(activityFile$steps))
if (noOfStepNAs > 0) {
    stepWord <- "are"
} else {
    stepWord <- "is"
}
noOfDateNAs <- sum(is.na(activityFile$data))
```

```
## Warning: is.na() applied to non-(list or vector) of type 'NULL'
```

```r
if (noOfDateNAs > 0) {
    dateWord <- "are"
} else {
    dateWord <- "is"
}

noOfIntervalNAs <- sum(is.na(activityFile$interval))
if (noOfIntervalNAs > 0) {
    intervalWord <- "are"
} else {
    intervalWord <- "is"
}
```

The no of steps with NA values are 2304.  
The no of dates with NA values is 0.  
The no of steps with NA values is 0.


```r
newActFile <- activityFile
noOfRows <- nrow(newActFile)
for (iH in 1:noOfRows) {
        
        if ( is.na(newActFile$steps[iH]) ) { 
            
            newActFile$steps[iH] = averageData[averageData$interval == newActFile$interval[iH],2]
            
        }
    }
hist(newActFile$steps)
```

![plot of chunk replaceNAs](figure/replaceNAs.png) 


```r
newStepsSummary <- summary(newActFile$steps)
medianOfNewSteps <- newStepsSummary[["Median"]]
meanOfNewSteps <- newStepsSummary[["Mean"]]
```


The median of the no NAs-left-in-the-datasert steps data is 0 and the mean is 37.4. As you can see there is no difference from the original data and that is to be expected due to the strategy that I followed to fill in the NAs with the average of that five minute interval!

### plotting weekdays / weekends against steps


```r
## change dates from class FACTOR to class DATE
newActFile$date <- as.Date(newActFile$date, format="%Y-%m-%d")
## add a column of the weekdays
newActFile <- cbind(newActFile, weekdays(newActFile$date))

colnames(newActFile)[4] <- "WeekDay"

melt <- melt(newActFile,id=c("interval","WeekDay"),measure.vars="steps")

average2Data <- dcast(melt,interval + WeekDay ~ variable,mean)

daysOfWeek <- c("Monday","Tuesday","Wednesday","Thursday","Friday")

average2Data = within(average2Data, {
     weekendOfWeekDay = ifelse(WeekDay %in% daysOfWeek, "weekday", "weekend")
 })

xyplot(average2Data$steps~average2Data$interval|average2Data$weekendOfWeekDay, 
       main="", 
        ylab="Number of Steps", xlab="interval",layout=(c(1,2)),type=c("l"))
```

![plot of chunk weekdaysAndDates](figure/weekdaysAndDates.png) 
