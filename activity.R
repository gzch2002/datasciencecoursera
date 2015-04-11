library(dplyr)
library(ggplot2)
activity<-read.csv("activity.csv")
sum(is.na(activity))
#extract the rows with NAs, and remove the steps column
act_NA<-filter(activity,is.na(steps))
act_NA<-select(act_NA,2,3)
#activity now contains no NA
activity <- na.omit(activity)

activity_by_day<-group_by(activity,date)
daily_sum<-summarize(activity_by_day,total_steps=sum(steps))
quartz()
#hist(daily_sum$total_steps, breaks=19)
mean_steps<-mean(daily_sum$total_steps)
median_steps<-median(daily_sum$total_steps)
#> mean_steps
#[1] 10766.19
#> median_steps
#[1] 10765
activity_by_interval<-group_by(activity,interval)
mean_interval<-summarize(activity_by_interval,mean_steps=mean(steps))
#with(mean_interval, plot(interval,mean_steps, type = "l", main="Time series plot of the 
#                         5-minute interval and the average steps of all days", 
#                         ylab="Average Steps of All Days", xlab="5-minute intervals"))
mean_interval[mean_interval$mean_steps==max(mean_interval$mean_steps),]
#merge the DF with no steps with the mean_interval table. Now the steps are replaced with 
#the mean for that 5-minute interval
act_NA<-merge(act_NA,mean_interval, by="interval")
act_NA<-arrange(act_NA, date, interval,mean_steps)
colnames(act_NA)[3] <- "steps"
#change the column order to the original order
act_NA<-act_NA[c(3,2,1)]
#combine the NA_replaced table with NA_removed table
data<-rbind(act_NA, activity)
data<-arrange(data, date, interval,steps)
#draw hist of daily steps for the new dataset with NAs filled in
data_by_day<-group_by(data,date)
new_daily_sum<-summarize(data_by_day,total_steps=sum(steps))
hist(new_daily_sum$total_steps, breaks=13)
new_mean_steps<-mean(new_daily_sum$total_steps)
new_median_steps<-median(new_daily_sum$total_steps)
#> new_mean_steps
#[1] 10766.19
#> new_median_steps
#[1] 10766.19
data$date<-as.Date(data$date)
data$Day_in_week<-ifelse((weekdays(data$date) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")), 
                         "weekday","weekend")
data_by_interval<-group_by(data,Day_in_week,interval)
mean_data_interval<-summarize(data_by_interval,mean_steps=mean(steps))
ggplot(mean_data_interval, aes(x=interval, y=mean_steps)) + 
  geom_line(color="violet") + 
  facet_wrap(~ Day_in_week, nrow=2, ncol=1) +
  labs(x="Interval", y="Number of steps") +
  theme_bw()
