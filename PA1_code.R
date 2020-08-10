## Downloading, unziping, and loading data into RStudio
filename <- "activity.zip"
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(fileURL, filename)
}
if (file.exists("activity.zip")) { 
        unzip(filename) 
}

activity <- read.csv("activity.csv")

## Cleaning data for analysis
activity$date <- as.Date(activity$date, format="%Y-%m-%d")

## Calculating and plotting mean number of steps taken per day
activity1 <- activity[!is.na(activity$steps),]
stepsums <- with(activity, tapply(steps, as.factor(activity$date), sum, na.rm = TRUE))

hist(stepsums, 
     main = "Total Number of Steps Taken per Day", 
     xlab = "Total Step Count",
     ylab = "Frequency")

summary(stepsums)

## Calculating and plotting average daily activity pattern and interval with max steps
intervalmeans <- tapply(activity1$steps, activity1$interval, FUN=mean)
intervals <- levels(as.factor(activity1$interval))

plot(intervals, intervalmeans, 
     type = "l", 
     main = "Time Series Plot of Average Steps Taken",
     xlab = "Interval",
     ylab = "Average Steps Taken")

df_int <- data.frame(intervalmeans, intervals)
df_int[df_int$intervalmeans == max(df_int$intervalmeans), ][2]

## Finding missing values, calculating average number of steps and imputing into NAs
activity2 <- activity[is.na(activity$steps),]
length(activity2$steps)
meansteps <- with(activity2, tapply(steps, activity2$interval, mean))
activity2$steps <- meansteps

## Making new dataframe with fixed NAs
activity_fixed <- rbind(activity1, activity2)
activity_fixed <- activity_fixed[order(activity_fixed$date),]

## Replotting steps taken per day with new dataframe and calulating median/mean
stepsums_fixed <- with(activity_fixed, tapply(steps, as.factor(activity_fixed$date), sum, na.rm = TRUE))
hist(stepsums_fixed, 
     main = "Total Number of Steps Taken per Day", 
     xlab = "Total Step Count",
     ylab = "Frequency")

summary(stepsums)
summary(stepsums_fixed)

## Adding day and weekday columns, indicating day of the week and 
activity_fixed$days <- weekdays(activity$date)

weekend <- grep("Saturday|Sunday", activity_fixed$days, ignore.case = TRUE)
weekend_dt <- activity_fixed[weekend, ]
weekend_dt$weekday <- "weekend"

weekdays <- grep("Monday|Tuesday|Wednesday|Thursday|Friday", activity_fixed$days, ignore.case = TRUE)
weekdays_dt <- activity_fixed[weekdays, ]
weekdays_dt$weekday <- "weekday"

activity_fixed <- rbind(weekend_dt, weekdays_dt)

