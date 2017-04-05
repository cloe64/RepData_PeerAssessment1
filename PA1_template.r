
############Q1.Reading Data############
data<-read.csv("activity.csv")
str(data)
library(ggplot2)
library(dplyr)
data$date<-as.Date(data$date)
##########Q2.Histogram of Total Num of Steps Per Day###############
data_Q1<-data%>%group_by(date)%>%summarize(steps_total=sum(steps,na.rm=T))%>%select(date,steps_total)
hist(data_Q1$steps_total,breaks=seq(0,25000,2500),ylim=c(0,20),col="blue",xlab="Total num of Steps",main="Histogram of Total num of steps per Day")
##########Q3.Mean and median number of steps taken each day###########
mean_steps<-mean(data_Q1$steps_total,na.rm=T)
median_steps<-median(data_Q1$steps_total,na.rm=T)
#########Q4.Time series plot of the average number of steps taken########
data_Q4<-data%>%group_by(interval)%>%summarize(steps_average=mean(steps,na.rm=T))%>%select(interval,steps_average)
ggplot(data_Q4,aes(interval,steps_average))+geom_line(col="blue")+xlab("5-minute Interval")+ylab("Average Steps")+
  ggtitle("Average Number of Steps")+theme(plot.title = element_text(hjust = 0.5))
#########Q5.5-minute interval that on average contain the maximum number of steps########
max_index<-which.max(data_Q4$steps_average)
max_interval<-data_Q4[max_index,]$interval
#########Q6.Code to describe and show a strategy for imputing missing data################
#####Get Percentage of Missing Value###################################################
per_step_mis<-mean(is.na(data$steps))
summary(data)
#####Filled Missing Values with Mean for that 5 minutes interval#########################
data_miss<-data
for (i in 1:dim(data_miss)[1]){
  if (is.na(data_miss[i,]$steps)){
    data_miss[i,]$steps<-data_Q4[data_Q4$interval==data_miss[i,]$interval,]$steps_average
  }
}
data_miss_hist<-data_miss%>%group_by(date)%>%summarize(steps_total=sum(steps))%>%select(date,steps_total)
##########Q7.Histogram of total number of steps per day with missing value imputed######
hist(data_miss_hist$steps_total,breaks=seq(0,25000,2500),ylim=c(0,20),col="blue",xlab="Total num of Steps",main="Histogram of Total num of steps per Day")
##########Mean and Median of total number steps per day after missing value was imputed####
mean_steps_miss<-mean(data_miss_hist$steps_total,na.rm=T)
median_steps_miss<-median(data_miss_hist$steps_total,na.rm=T)
##########Q8.Average number of steps per 5 Minutes interval across Weekdays and Weekend####
data_miss$day<-ifelse(weekdays(data_miss$date)%in%c("Monday","Tuesday","Thursday","Wednesday","Friday"),"Weekday","Weekend")
average_step<-data_miss%>%group_by(interval,day)%>%summarize(steps_mean=mean(steps))%>%select(day,steps_mean,interval)
library(lattice)
xyplot(steps_mean~interval|day,average_step,
       type="l",
       lwd=1,
       xlab="Interval",
       ylab="Number of Steps",
       layout=c(1,2))

