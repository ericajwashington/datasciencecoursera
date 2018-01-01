#Reproducible Research Course Project 1 - Activity Monitoring
##Submitter @ericajwashington

###1. Load the data (i.e., read.csv)
activity <- read.csv("activity.csv",header=T)

###2. What is the meant total of steps taken per day?
####2a.Calculate the total number of steps taken per day
totalstepsperday <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
totalstepsperday

####2b. Make a histogram of the total number of steps taken each day.
hist(totalstepsperday$steps, 
     main="Total Steps per Day", 
     xlab="Number of Steps per Day", 
     ylab = "Interval",
     col="orange",
     breaks=50)

####2c. Calculate and report the mean and median of the total number of steps taken per day.
msteps <- mean(totalstepsperday$steps)
msteps
#####mean steps = 10766.19

medsteps <- median(totalstepsperday$steps)
medsteps
#####median steps = 10765

summary(totalstepsperday)

###3. What is the average daily activity pattern?
####3a. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
fivemin <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
plot(x = fivemin$interval, 
     y = fivemin$steps, 
     type = "l", 
     col = "orange",
     xlab = "5-minute Intervals",
     ylab = "Average Steps Taken ~ Days",
     main = "Average Daily Activity Pattern")

####3b. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxsteps <- fivemin$interval[which.max(fivemin$steps)]
maxsteps
#### 835

###4. Imputing missing values
####4a. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
impute <- sum(is.na(activity$steps))
impute
####2304

####4b. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
####Strategy = Replace NA values with the mean results for five minute intervals

####4c. Create a new dataset that is equal to the original dataset but with the missing data filled in.
activity2 <- activity
nas <- is.na(activity2$steps)
avg_interval <- tapply(activity2$steps, activity2$interval, mean, na.rm=TRUE, simplify = TRUE)
activity2$steps[nas] <- avg_interval[as.character(activity2$interval[nas])]
names(activity2)
sum(is.na(activity2)) 

####4d. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 
####What is the impact of imputing missing data on the estimates of the total daily number of steps?
totalstepsperday2 <- aggregate(steps ~ date, data = activity2, FUN = sum, na.rm = TRUE)
totalstepsperday2
hist(totalstepsperday2$steps, 
     main = "Total Steps per Day (no-NA)", 
     xlab = "Number of Steps per Day", 
     ylab = "Interval",
     col="green",
     breaks=50)
summary(totalstepsperday)
####Original Dataset: Mean=10766; Median=10765
summary(totalstepsperday2)
####New dataset: mean-10766; Median=10766
####Impact: the second dataset is normally distributed where the mean and median are the same. 

###5. Are there differences in activity patterns between weekdays and weekends?
####5a. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activity2<- activity2%>%
  mutate(typeofday= ifelse(weekdays(activity2$date)=="Saturday" | weekdays(activity2$date)=="Sunday", "Weekend", "Weekday"))
head(activity2)

####5b. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub 
####repository to see an example of what this plot should look like using simulated data.
fivemin2<- aggregate(steps ~ interval, data = activity2, FUN = mean, na.rm = TRUE)
head(fivemin2)
ggplot(activity2, aes(x =interval , y=steps, color=typeofday)) +
  geom_line() +
  labs(title = "Ave Daily Steps (type of day)", x = "Interval", y = "Total Number of Steps") +
  facet_wrap(~ typeofday, ncol = 1, nrow=2)