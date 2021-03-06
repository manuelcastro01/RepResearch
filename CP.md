---
title: "RR - Course Project 1"
author: "Manuel Castro"
date: "10/1/2022"

output:
  md_document:
    variant: markdown_github

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Step 1

Loading and processing the data

```{r echo=TRUE}
library(ggplot2)
library(dplyr)
library(lubridate)
data <- read.csv("activity.csv")
head(data)
```

Calculating the mean of steps column.

```{r echo=TRUE}
mean(data$steps, na.rm = TRUE)
data$Day <- wday(as.Date(data$date))
d <- data %>% group_by(wday(as.Date(date)), steps) %>% summarise(TotalSteps = sum(steps))
ggplot() +   geom_histogram(aes(x=d$steps, fill = as.character(d$`wday(as.Date(date))`))) + xlab("Steps") + ylab("Frecuency") +ggtitle("Histogram of steps per day") + scale_fill_discrete(name = "Week Day")
```


```{r echo=TRUE}
names(d)[1] <- "Day"
```
#### Reporting the mean steps per day


```{r echo=TRUE}
d %>%
      group_by(Day) %>%
      summarise(Mean = mean(steps, na.rm = T))
```
#### Reporting the median steps per day


```{r echo=TRUE}
d %>%
      group_by(Day) %>%
      summarise(Median = median(steps, na.rm = T))
```

#### Average steps per interval
```{r}
data %>% group_by(interval) %>% summarise(Mean = mean(steps, na.rm = T)) %>% 
  ggplot() + geom_line(aes(x = interval, y  =Mean)) + ggtitle("Interval vs. Average steps")
```


#### The interval with the max average steps
```{r}
mx <- data %>% group_by(interval) %>% summarise(Mean = mean(steps, na.rm = T))
mx[which(mx$Mean == max(mx$Mean)),]

```
```{r}

nasteps <- which(is.na(data$steps))
nadate <- which(is.na(data$date))
nainterval <- which(is.na(data$interval))

print(paste0(length(complete.cases(data)), " rows are fully complete"))
print(paste0("There are ",length(nasteps)," missing values in Steps column"))
print(paste0("There are ",length(nadate)," missing values in Date column"))
print(paste0("There are ",length(nainterval)," missing values in Interval column"))

```
#### Imputing missing values and creating the new DataSet, replaced NAs by the mean
```{r}

newsteps <- data$steps  
newsteps[is.na(newsteps)] <- mean(data$steps, na.rm = TRUE)

newdf <- as.data.frame(cbind(newsteps, data$date, data$interval, data$Day))
names(newdf) <- c("Steps", "Date", "Interval", "Day")
head(newdf)

```

```{r}

auxdf <- newdf %>% group_by(wday(as.Date(Date)), Steps) %>% summarise(TotalSteps = sum(as.numeric(Steps)))

names(auxdf) <- c("Day", "Steps", "Total")
ggplot() + geom_histogram(aes(x = as.numeric(auxdf$Steps), fill = as.character(auxdf$Day)))+ xlab("Steps") + ylab("Frecuency") +ggtitle("Histogram of steps per day with fixed Na") + scale_fill_discrete(name = "Week Day")

```
#### New timeseries plot taking into account if the day is weekend or not
```{r}

d <- mutate(newdf, Weekday = ifelse(Day >= 6, "Weekend", "Weekday"))

d <- d %>% group_by(Interval, Weekday) %>% summarise(Mean = mean(as.numeric(Steps)))
ggplot(d) + geom_line(aes(x = as.numeric(Interval), y = Mean, col= Weekday), size = 1) + ggtitle("Interval vs. Average steps") + xlab("Interval") + ylab("Average Steps")




```
