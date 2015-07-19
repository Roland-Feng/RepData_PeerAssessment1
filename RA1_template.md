title: "PA1_template"

output: html_document

by Roland Feng

Loading and preprocessing the data

First we load the data with read.csv(). Here we save it as dataStep, and we could see its structure:

dataStep<-read.csv("activity.csv")
str(dataStep)
I use split() to seperate the date.

dataStep2<-split(dataStep,dataStep$date)
What is mean total number of steps taken per day?

Notice that there are 61 days. So we use a for here:

dataStepDay<-1:61
for (i in 1:61){
        dataStepDay[i]<-sum(dataStep2[[i]]$steps)
}
Then we plot the histogram

hist(dataStepDay,xlab="Number of Steps",col="red")
The mean and meidan is calculated by

dataStepDay<-dataStepDay[!is.na(dataStepDay)]
mean(dataStepDay)
median(dataStepDay)
and they are 10766.19 and 10765.

What is average daily activity pattern?

First, we need to calculate the mean of the steps of dates by intervals. There are 288 intervals

dataStep3<-split(dataStep,dataStep$interval)
dataStepInterval<-1:288
for(i in 1:288){
        x<-dataStep3[[i]]$steps
        x<-x[!is.na(x)]
        dataStepInterval[i]<-mean(x)
}
Then we plot

Interval<-unique(dataStep$interval)
plot(Interval,dataStepInterval,type="l")
Interval[which.max(dataStepInterval)]
The 835's 5-minute interval,on average across all the days in the dataset, contains the maximum number of steps

Imputing missing values

The number of NAs can be calculated by

dataStep4<-dataStep$steps
length(dataStep4[is.na(dataStep4)])
So there are 2304 NAs

We use the mean for that 5-minute interval to fill the dataset,save it as dataStepNew

dataStepNew<-dataStep
for(i in 1:nrow(dataStepNew)){
        if (is.na(dataStepNew[i,1])){
                dataStepNew[i,1]<-dataStepInterval[which(Interval==dataStepNew[i,3])]
        }
}
sum(is.na(dataStepNew$steps))
Now we calculate the steps per day

dataStepNew2<-split(dataStepNew,dataStepNew$date)
dataStepDayNew<-1:61
for (i in 1:61){
        dataStepDayNew[i]<-sum(dataStepNew2[[i]]$steps)
}
hist(dataStepDayNew,xlab="Number of Steps",col="red")
mean(dataStepDayNew)
median(dataStepDayNew)
As we can see, the frequency between 10000 and 15000 grows a lot The mean is 10766.19 The median is 10766.19

Are there differences in activity patterns between weekdays and weekends?

First we add the weekday or weekend

weekday <- c("星期一", "星期二", "星期三", "星期四", "星期五")
dataStepNew$day<-as.factor(ifelse(is.element(weekdays(as.Date(as.character(dataStepNew$date))),weekday), "Weekday","Weekend"))
dataStepIntervalNew<- aggregate(steps~interval+day,dataStepNew, mean)
library(lattice)
xyplot(dataStepIntervalNew$steps~dataStepIntervalNew$interval|dataStepIntervalNew$day, layout=c(1,2), type="l")
Forgive me that the language of my R software is Chinese, so the names of weekdays are in Chinese. "星期一" is Monday, "星期二" is Tuesday, "星期三" is Wednesday, "星期四" is Thursday, "星期五" is Friday.
