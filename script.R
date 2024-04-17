##  Load Data
data <- read.csv("activity.csv")

data2 <- data[!(is.na(data$steps)), ]
steps_per_day <- aggregate(steps ~ date, data=data2, FUN = sum)

hist(steps_per_day$steps, main="Histogram of the total number of steps taken each day", xlab="Steps", ylab="Frequency")


mean_steps_per_day <- mean(steps_per_day$steps)
median_steps_per_day <- median(steps_per_day$steps)
mean_steps_per_day
median_steps_per_day

my_mean <- function(x) mean(x, na.rm = TRUE) 
data3 <- aggregate(steps ~ interval, data, FUN=my_mean)
head(data3)

plot(y=data3$steps,x=data3$interval, type="l", xlab="Intervals", ylab="Steps")

index_max_steps <- which.max(data3$steps)
interval_max_steps <- data3$interval[index_max_steps]
interval_max_steps
max(data3$steps)

data4 <- subset(data, is.na(data$steps))
dims <- dim(data4)
nrows_na <- dims[1]
nrows_na

data_nona <- data

for (i in 1:nrow(data_nona)){
  if (is.na(data_nona$steps[i])){
    interval <- data_nona$interval[i]
    index <- which(data3$interval == interval)
    mean_val <- data3$steps[index]
    data_nona$steps[i] <- mean_val
  }
}

head(data_nona)


data4 <- aggregate(steps ~ date, data_nona, FUN=sum)
hist(data4$steps, main="Histogram of the total number of steps taken each day", xlab="Steps", ylab="Frequency")


mean_steps <- mean(data4$steps)
median_steps <- median(data4$steps)
mean_steps
median_steps

data5 <- data_nona
data5$date <- as.Date(data5$date)
data5$day <- weekdays(data5$date)

for (i in 1:nrow(data5)){
  if (data5$day[i] %in% c("Saturday", "Sunday")){
    data5$weekday[i] <- "weekend"
  }
  else {
    data5$weekday[i] <- "weekday"
  }
}

fct_days <- factor(data5$weekday)
summary(fct_days)

wknd_data <- data5[data5$weekday == "weekend",]
wkday_data <- data5[data5$weekday == "weekday",]

wknd_data <- aggregate(steps ~ interval, wknd_data, mean)
wkday_data <- aggregate(steps ~ interval, wkday_data, mean)

par(mfrow=c(2, 1))
plot(wknd_data$interval, wknd_data$steps, type="l")
plot(wkday_data$interval, wkday_data$steps, type="l")
