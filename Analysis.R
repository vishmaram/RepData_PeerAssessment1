unzip("activity.zip", exdir = "data/", overwrite = TRUE)


activity <- read.csv("data/activity.csv")
head(activity)

naLogical <- !is.na(activity$steps)
activityEdited <- activity[naLogical,]

sum(is.na(activity$steps))

activityByDay <- aggregate(activityEdited$steps,list(activityEdited$date), sum)
names(activityByDay) <- c("Date", "Total Steps")

hist(activityByDay$`Total Steps`, main="Total number of steps taken each day")

summary(activityByDay$`Total Steps`)

activityByInterval <- aggregate(activityEdited$steps, list(activityEdited$interval), mean)
head(activityByInterval)
names(activityByInterval) <- c("Interval", "Avg. Steps")

plot(activityByInterval$Interval,activityByInterval$`Avg. Steps`,type = "l",xlab="Interval",ylab="Avg. Steps Taken")
title(main = "Average Steps by interval")

which.max(activityByInterval$`Avg. Steps`)
activityByInterval[104,]

activityByInterval[activityByInterval$`Avg. Steps` == 206.000,]

activityByDayAvg <- aggregate(activityEdited$steps,list(activityEdited$date), mean)
names(activityByDayAvg) <- c("Date", "Mean Steps")

head(activityByDayAvg)

activityMerged <- merge(activity,activityByInterval,by.x = "interval", by.y="Interval")
head(activityMerged)

activityMerged[is.na(activityMerged$steps),2] <- activityMerged[is.na(activityMerged$steps),4]

newActivitySet <- activityMerged


newActivityByDay <- aggregate(newActivitySet$steps,list(newActivitySet$date), sum)
names(newActivityByDay) <- c("Date", "Total Steps")

hist(newActivityByDay$`Total Steps`)

summary(newActivityByDay$`Total Steps`)

newActivitySet <- cbind(newActivitySet,weekdays(as.POSIXlt(newActivitySet$date)),"Weekend")

head(newActivitySet)

names(newActivitySet) <- c("Interval",  "Steps","Date","AvgSteps", "WeekDay","DayType")

newActivitySet$DayType <- as.character(newActivitySet$DayType)

newActivitySet[newActivitySet$WeekDay != "Sunday" & newActivitySet$WeekDay !="Saturday", 6] <- "Weekday"

newActivitySet$DayType <- as.factor(newActivitySet$DayType)

 

newActivitySetByInterval <- aggregate(newActivitySet$Steps, list(newActivitySet$Interval, newActivitySet$DayType), mean)

head(newActivitySetByInterval)

names(newActivitySetByInterval)  <- c("Interval", "DayType", "Average")

library(ggplot2)


ggplot(newActivitySetByInterval, mapping = aes(Interval,Average, col = DayType,title='Average Steps by interval by day type')) +
  ylab("Average Steps") +
  geom_point(size=3)+ geom_smooth(method="lm") + facet_grid(facets = DayType~.)
