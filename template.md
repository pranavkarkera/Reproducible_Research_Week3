---
title: "Activity Monitoring"
author: "Pranav Karkera"
date: "`18-04-2024`"
output: html_document
---

# Loading Data

```{r}
data <- read.csv("activity.csv")
```


### Calculate the Total Number of Steps Taken per Day
```{r}
data2 <- data[!(is.na(data$steps)), ]
steps_per_day <- aggregate(steps ~ date, data=data2, FUN = sum)

```

### Histogram of the Total Number of Steps Taken per Day
```{r}
hist(steps_per_day$steps, main="Histogram of the total number of steps taken each day", xlab="Steps", ylab="Frequency")

```

### Calculate the Mean and Median of the Total Number of Steps Taken per Day
```{r}
mean_steps_per_day <- mean(steps_per_day$steps)
median_steps_per_day <- median(steps_per_day$steps)
mean_steps_per_day
median_steps_per_day
```   



### Make a Time Series plot of the 5-Minute Interval (X-Axis) and the Average Number of steps taken, averaged across all days (Y-Axis)
```{r}
my_mean <- function(x) mean(x, na.rm = TRUE) 
data3 <- aggregate(steps ~ interval, data, FUN=my_mean)
head(data3)

```

```{r}
plot(y=data3$steps,x=data3$interval, type="l", xlab="Intervals", ylab="Steps")

```

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
index_max_steps <- which.max(data3$steps)
interval_max_steps <- data3$interval[index_max_steps]
interval_max_steps
max(data3$steps)
```


# Imputing missing values

### Calculate and report the total number of missing values in the dataset (total number of rows with NAs)
```{r}
data4 <- subset(data, is.na(data$steps))
dims <- dim(data4)
nrows_na <- dims[1]
nrows_na
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```{r}
# I am using the mean for the interval
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

```


### Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
head(data_nona)

```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
data4 <- aggregate(steps ~ date, data_nona, FUN=sum)
hist(data4$steps, main="Histogram of the total number of steps taken each day", xlab="Steps", ylab="Frequency")


mean_steps <- mean(data4$steps)
median_steps <- median(data4$steps)
mean_steps
median_steps
```
### Slight increase in average


# Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
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

```

### Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r}
wknd_data <- data5[data5$weekday == "weekend",]
wkday_data <- data5[data5$weekday == "weekday",]

wknd_data <- aggregate(steps ~ interval, wknd_data, mean)
wkday_data <- aggregate(steps ~ interval, wkday_data, mean)

par(mfrow=c(2, 1))
plot(wknd_data$interval, wknd_data$steps, type="l")
plot(wkday_data$interval, wkday_data$steps, type="l")
```