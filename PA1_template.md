Markdown file for Week 1 project
==================================
# What is the mean total number of steps taken per day?
## Set options
Set global options

```r
library(knitr)
opts_chunk$set(echo=TRUE)
```
## Read data
First we read the file

```r
setwd("~/Dropbox/R/Reproducible Research/Assessment 1")
data<-read.csv("activity.csv")
```
## Reshape data
In order to calculate total steps per day, and also mean and median, we need to reshape the data.

```r
library(reshape2)
data.melt<-melt(data,id=c("date"),measure.vars=c("steps"),na.rm=TRUE)
data.dcast.sum<-dcast(data.melt,date~variable,sum)
```
## Plot the graph and print report
Plot the sum steps per day

```r
hist(data.dcast.sum$steps,main="Steps per Day",xlab="Steps")
```

![plot of chunk plot parametres](figure/plot parametres.png) 

```r
steps.mean<-mean(data.dcast.sum$steps)
steps.median<-median(data.dcast.sum$steps)
report<-cbind(steps.mean,steps.median)
print(report)
```

```
##      steps.mean steps.median
## [1,]      10766        10765
```
# What is the average daily activity pattern?
## Melt the data based on interval then plot

```r
data.melt.2<-melt(data,id=c("interval"),measure.var=c("steps"),na.rm=TRUE)
data.dcast.2<-dcast(data.melt.2,interval~variable,mean)
plot(data.dcast.2$interval,data.dcast.2$steps,type="l",main="Average Daily Activity Pattern",xlab="5 min interval",ylab="steps")
```

![plot of chunk plot Average Daily Activity](figure/plot Average Daily Activity.png) 
## Order by steps

```r
steps.rank<-data.dcast.2[order(data.dcast.2$steps,decreasing=TRUE),]
steps.max<-steps.rank[1,1]
print(steps.max)
```

```
## [1] 835
```
The 5-minute interval 835, on average across all the days in the dataset, contains the maximum number of steps.
## Imputing missing values
### Calculate and report the total number of missing values

```r
NA.cal<-colSums(is.na(data))
No.NA<-NA.cal[1]
```
There are 2304 missing values in the dataset
### Fill in NA values

```r
data.fill<-data
for (i in 1:length(data$steps)){
        if(is.na(data.fill$steps[i])){
                interval.id<-data.fill$interval[i]
                interval.mean<-data.dcast.2[data.dcast.2$interval==interval.id,2]
                data.fill$steps[i]<-interval.mean
        }
}
```
## Plot a histogram and generate a report on the new dataset

```r
data.fill.melt<-melt(data.fill,id=c("date"),measure.vars=c("steps"))
data.fill.dcast.sum<-dcast(data.fill.melt,date~variable,sum)

hist(data.fill.dcast.sum$steps,main="Steps per Day (NA filled with mean for each 5-minute interval)",xlab="Steps")
```

![plot of chunk plot a histogram with NA filled](figure/plot a histogram with NA filled.png) 

```r
steps.mean.2<-mean(data.fill.dcast.sum$steps)
steps.median.2<-median(data.fill.dcast.sum$steps)
report.NAfilled<-cbind(steps.mean.2,steps.median.2)
report.combined<-rbind(report,report.NAfilled)
row.names(report.combined)<-c("Original","NA filled with mean for the 5-minute interval")
print(report.combined)
```

```
##                                               steps.mean steps.median
## Original                                           10766        10765
## NA filled with mean for the 5-minute interval      10766        10766
```
NA values have a small effect in median but not mean calculation
# Are there differences in activity patterns between weekdays and weekends?

```r
for (i in 1:length(data.fill$date)){
        date.id<-as.Date(data.fill$date[i])
        weekday.id<-weekdays(date.id)
        data.fill$Weekday[i]<-weekday.id
}

for(i in 1:length(data.fill$date)){
        if(data.fill$Weekday[i] %in% c("Saturday","Sunday")){
                data.fill$WeekdayorWeekend[i]<-c("Weekend")
        }
        else{
                data.fill$WeekdayorWeekend[i]<-c("Weekday")
        }
}

data.split.day<-split(data.fill,data.fill$WeekdayorWeekend)
input<-data.frame()
mean.interval<-function(input){
        id.melt<-melt(input,id=c("interval"),measure.vars=c("steps"))
        id.dcast<-dcast(id.melt,interval~variable,mean)
}
output.day<-lapply(data.split.day,mean.interval)

par(mfrow=c(1,2))
with(output.day,{
        plot(output.day$Weekday$interval,output.day$Weekday$steps,type="l",main="Weekday",xlab="5 min interval",ylab="steps")
        plot(output.day$Weekend$interval,output.day$Weekend$steps,type="l",main="Weekend",xlab="5 min interval",ylab="steps")
})
```

![plot of chunk plot Average Daily Activity(Weekday vs Weekdend)](figure/plot Average Daily Activity(Weekday vs Weekdend).png) 
