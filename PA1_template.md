## Loading and preprocessing the data

The following section of code loads the data and transforms the date variable from character to Date format. The processed data is stored in a data frame named activity. We assume that the raw data file activity.csv is already in the working directory.


```r
activity<-read.csv("activity.csv",header=TRUE,quote="\"",na.strings="NA",colClasses=c("numeric","character","numeric"))
activity$obs_dates<-as.Date(activity$date,"%Y-%m-%d")
```

## What is mean total number of steps taken per day?

Using the processed data, we sum the total number of steps by their dates. The resulting histogram of the frequencies of the daily total number of steps is shown below, along with the mean and median.


```r
dailytotalsteps<-tapply(activity$steps,activity$obs_dates,sum,na.rm=TRUE)
dailytotalsteps2<-dailytotalsteps[complete.cases(dailytotalsteps)]
hist(dailytotalsteps2,xlab="Daily total number of steps",main="Histogram of Daily Total Number of Steps")
```

![plot of chunk meanandmedian](figure/meanandmedian-1.png) 

```r
dailytotalmean<-mean(dailytotalsteps2)
print(dailytotalmean)
```

```
## [1] 9354.23
```

```r
dailytotalmedian<-median(dailytotalsteps2)
print(dailytotalmedian)
```

```
## [1] 10395
```
We see from the output above that the mean and median are 9354.2295082 and 1.0395 &times; 10<sup>4</sup> respectively.

## What is the average daily activity pattern?

Here, we use the processed data frame once again to construct a plot of average daily activity vs the interval. However we need to first convert the interval, given in base-60 in the two right-most digits to base-10. For example, the interval 2020 should be converted to 2033.3333. The result is store in a new variable named interval2. Following that, we compute the mean of the number of steps by interval across all days, and plot the time series.  


```r
activity$interval2<-(activity$interval %/% 100)*100 + 100*(activity$interval %% 100)/60
avgsteps<-tapply(activity$steps,activity$interval2,mean,na.rm=TRUE)
interval10<-as.numeric(names(avgsteps))
avgstepsvect<-as.vector(avgsteps)
plot(x=interval10,y=avgstepsvect,type="l",xlab="Interval",ylab="Average number of steps",main="Average daily activity pattern")
```

![plot of chunk avgdailyactivity](figure/avgdailyactivity-1.png) 


We can then calculate the interval where the maximum activity occurs.




```r
maxavgsteps<-max(avgstepsvect)
maxavgstepsint<-interval10[which(avgstepsvect==max(avgstepsvect))]
maxavgstepsint_min<-maxavgstepsint %/% 100
maxavgstepsint_sec<-((maxavgstepsint %% 100)/100)*60

print(maxavgsteps)
```

```
## [1] 206.1698
```

```r
print(maxavgstepsint_min)
```

```
## [1] 8
```

```r
print(maxavgstepsint_sec)
```

```
## [1] 35
```
The maximum average activity is 206.1698113 and this occurs at the interval 8 min 35 sec.

## Imputing missing values


```r
isNAnum<-sum(is.na(activity$steps))
print(isNAnum)
```

```
## [1] 2304
```
There are 2304 missing values in the steps variable.  

Now lets impute the missing values by using the mean for the 5-min intervals.


```r
intervals_char<-unique(activity$interval)
for (x in 1:Nrows){
if(is.na(activity$steps[x])){
imputedsteps[x]<-avgstepsvect[which(intervals_char==activity$interval[x])]
}
else {
imputedsteps[x]<-activity$steps[x]
}
}
```
Next, we create a new data frame using the imputed values. Similar to the original, our new data frame has three variables.


```r
activity_new<-data.frame(steps=imputedsteps, date=activity$date, interval=activity$interval)
head(activity_new)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Finally, we plot the histogram and compute the mean and median.


```r
hist(dailytotalsteps_new,xlab="Daily total number of steps",main="Histogram of Daily Total Number of Steps (using imputed data)")
```

![plot of chunk new](figure/new-1.png) 

```r
dailytotalmean_new<-mean(dailytotalsteps_new)
print(dailytotalmean_new)
```

```
## [1] 10766.19
```

```r
dailytotalmedian_new<-median(dailytotalsteps_new)
print(dailytotalmedian_new)
```

```
## [1] 10766.19
```
The new mean and median are 1.0766189 &times; 10<sup>4</sup> and 1.0766189 &times; 10<sup>4</sup> respectively. We see that the mean and median have both increased, with a larger increment by percentage for the mean.

## Are there differences in activity patterns between weekdays and weekends?

We shall create a new factor variable wk with two levels "weekday" and "weekend". We have used the weekdays() function to identify the day of the week and assumed Saturdays and Sundays to be weekend days. This new factor variable has been added on to the new data frame, which has missing data imputed.


```r
wkdays<-weekdays(activity$obs_date)
wk<-sapply(wkdays, function(x) {if(x=="Sunday" || x=="Saturday") txt<-"weekend" else txt<-"weekday"; txt})
activity_new$interval2<-activity$interval2
activity_new$wk<-as.factor(wk)
head(activity_new)
```

```
##       steps       date interval interval2      wk
## 1 1.7169811 2012-10-01        0  0.000000 weekday
## 2 0.3396226 2012-10-01        5  8.333333 weekday
## 3 0.1320755 2012-10-01       10 16.666667 weekday
## 4 0.1509434 2012-10-01       15 25.000000 weekday
## 5 0.0754717 2012-10-01       20 33.333333 weekday
## 6 2.0943396 2012-10-01       25 41.666667 weekday
```

We have created a panel plot to compare the average number of steps take across weekdays and weekends.


```r
splitframe<-split(activity_new,wk)
avgsteps_wkday<-tapply(splitframe$weekday$steps,splitframe$weekday$interval2,mean)
avgsteps_wkend<-tapply(splitframe$weekend$steps,splitframe$weekend$interval2,mean)
par(mfrow=c(2,1))
plot(x=interval10, y=avgsteps_wkday, type="l",xlab="Interval", ylab="Number of steps",main="Weekday")
plot(x=interval10, y=avgsteps_wkend, type="l",xlab="Interval", ylab="Number of steps",main="Weekend")
```

![plot of chunk plotweekday](figure/plotweekday-1.png) 

