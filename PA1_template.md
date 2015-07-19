# PA1_template
by Roland Feng



##Loading and preprocessing the data

First we load the data with read.csv(). Here we save it as dataStep, and we could see its structure:

```r
dataStep<-read.csv("./activity.csv")
str(dataStep)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

I use split() to seperate the date.

```r
dataStep2<-split(dataStep,dataStep$date)
```

##What is mean total number of steps taken per day?

Notice that there are 61 days. So we use a **for** here:

```r
dataStepDay<-1:61
for (i in 1:61){
        dataStepDay[i]<-sum(dataStep2[[i]]$steps)
}
```
Then we plot the histogram

```r
hist(dataStepDay,xlab="Number of Steps",col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

The mean and meidan is

```r
dataStepDay<-dataStepDay[!is.na(dataStepDay)]
mean(dataStepDay)
```

```
## [1] 10766.19
```

```r
median(dataStepDay)
```

```
## [1] 10765
```

##What is average daily activity pattern?

First, we need to calculate the mean of the steps of dates by intervals. There are 288 intervals

```r
dataStep3<-split(dataStep,dataStep$interval)
dataStepInterval<-1:288
for(i in 1:288){
        x<-dataStep3[[i]]$steps
        x<-x[!is.na(x)]
        dataStepInterval[i]<-mean(x)
}
```

Then we plot

```r
Interval<-unique(dataStep$interval)
plot(Interval,dataStepInterval,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 


```r
Interval[which.max(dataStepInterval)]
```

```
## [1] 835
```

The 835's 5-minute interval,on average across all the days in the dataset, contains the maximum number of steps

##Imputing missing values

The number of NAs can be calculated by

```r
dataStep4<-dataStep$steps
length(dataStep4[is.na(dataStep4)])
```

```
## [1] 2304
```
So there are 2304 NAs

We use the mean for that 5-minute interval to fill the dataset,save it as dataStepNew

```r
dataStepNew<-dataStep
for(i in 1:nrow(dataStepNew)){
        if (is.na(dataStepNew[i,1])){
                dataStepNew[i,1]<-dataStepInterval[which(Interval==dataStepNew[i,3])]
        }
}
sum(is.na(dataStepNew$steps))
```

```
## [1] 0
```

Now we calculate the steps per day

```r
dataStepNew2<-split(dataStepNew,dataStepNew$date)
dataStepDayNew<-1:61
for (i in 1:61){
        dataStepDayNew[i]<-sum(dataStepNew2[[i]]$steps)
}
hist(dataStepDayNew,xlab="Number of Steps",col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

```r
mean(dataStepDayNew)
```

```
## [1] 10766.19
```

```r
median(dataStepDayNew)
```

```
## [1] 10766.19
```

As we can see, the frequency between 10000 and 15000 grows a lot
The mean is 10766.19
The median is 10766.19

##Are there differences in activity patterns between weekdays and weekends?

First we add the weekday or weekend

```r
weekday <- c("星期一", "星期二", "星期三", "星期四", "星期五")
dataStepNew$day<-as.factor(ifelse(is.element(weekdays(as.Date(as.character(dataStepNew$date))),weekday), "Weekday","Weekend"))
dataStepIntervalNew<- aggregate(steps~interval+day,dataStepNew, mean)
library(lattice)
xyplot(dataStepIntervalNew$steps~dataStepIntervalNew$interval|dataStepIntervalNew$day, layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
