## Step 1

Loading and processing the data

``` r
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
data <- read.csv("activity.csv")
head(data)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

Calculating the mean of steps column.

``` r
mean(data$steps, na.rm = TRUE)
```

    ## [1] 37.3826

``` r
data$Day <- wday(as.Date(data$date))
d <- data %>% group_by(wday(as.Date(date)), steps) %>% summarise(TotalSteps = sum(steps))
```

    ## `summarise()` has grouped output by 'wday(as.Date(date))'. You can override using the `.groups` argument.

``` r
ggplot() +   geom_histogram(aes(x=d$steps, fill = as.character(d$`wday(as.Date(date))`))) + xlab("Steps") + ylab("Frecuency") +ggtitle("Histogram of steps per day") + scale_fill_discrete(name = "Week Day")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 6 rows containing non-finite values (stat_bin).

![](CP1_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
names(d)[1] <- "Day"
```

#### Reporting the mean steps per day

``` r
d %>%
      group_by(Day) %>%
      summarise(Mean = mean(steps, na.rm = T))
```

    ## # A tibble: 7 x 2
    ##     Day  Mean
    ##   <dbl> <dbl>
    ## 1     1  222.
    ## 2     2  249.
    ## 3     3  224.
    ## 4     4  250.
    ## 5     5  232.
    ## 6     6  248.
    ## 7     7  248.

#### Reporting the median steps per day

``` r
d %>%
      group_by(Day) %>%
      summarise(Median = median(steps, na.rm = T))
```

    ## # A tibble: 7 x 2
    ##     Day Median
    ##   <dbl>  <dbl>
    ## 1     1   157 
    ## 2     2   130 
    ## 3     3   144 
    ## 4     4   156.
    ## 5     5   136 
    ## 6     6   168.
    ## 7     7   169

#### Average steps per interval

``` r
data %>% group_by(interval) %>% summarise(Mean = mean(steps, na.rm = T)) %>% 
  ggplot() + geom_line(aes(x = interval, y  =Mean)) + ggtitle("Interval vs. Average steps")
```

![](CP1_files/figure-markdown_github/unnamed-chunk-6-1.png)

#### The interval with the max average steps

``` r
mx <- data %>% group_by(interval) %>% summarise(Mean = mean(steps, na.rm = T))
mx[which(mx$Mean == max(mx$Mean)),]
```

    ## # A tibble: 1 x 2
    ##   interval  Mean
    ##      <int> <dbl>
    ## 1      835  206.

``` r
nasteps <- which(is.na(data$steps))
nadate <- which(is.na(data$date))
nainterval <- which(is.na(data$interval))

print(paste0(length(complete.cases(data)), " rows are fully complete"))
```

    ## [1] "17568 rows are fully complete"

``` r
print(paste0("There are ",length(nasteps)," missing values in Steps column"))
```

    ## [1] "There are 2304 missing values in Steps column"

``` r
print(paste0("There are ",length(nadate)," missing values in Date column"))
```

    ## [1] "There are 0 missing values in Date column"

``` r
print(paste0("There are ",length(nainterval)," missing values in Interval column"))
```

    ## [1] "There are 0 missing values in Interval column"

#### Imputing missing values and creating the new DataSet, replaced NAs by the mean

``` r
newsteps <- data$steps  
newsteps[is.na(newsteps)] <- mean(data$steps, na.rm = TRUE)

newdf <- as.data.frame(cbind(newsteps, data$date, data$interval, data$Day))
names(newdf) <- c("Steps", "Date", "Interval", "Day")
head(newdf)
```

    ##              Steps       Date Interval Day
    ## 1 37.3825995807128 2012-10-01        0   2
    ## 2 37.3825995807128 2012-10-01        5   2
    ## 3 37.3825995807128 2012-10-01       10   2
    ## 4 37.3825995807128 2012-10-01       15   2
    ## 5 37.3825995807128 2012-10-01       20   2
    ## 6 37.3825995807128 2012-10-01       25   2

``` r
auxdf <- newdf %>% group_by(wday(as.Date(Date)), Steps) %>% summarise(TotalSteps = sum(as.numeric(Steps)))
```

    ## `summarise()` has grouped output by 'wday(as.Date(Date))'. You can override using the `.groups` argument.

``` r
names(auxdf) <- c("Day", "Steps", "Total")
ggplot() + geom_histogram(aes(x = as.numeric(auxdf$Steps), fill = as.character(auxdf$Day)))+ xlab("Steps") + ylab("Frecuency") +ggtitle("Histogram of steps per day with fixed Na") + scale_fill_discrete(name = "Week Day")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](CP1_files/figure-markdown_github/unnamed-chunk-10-1.png) \#\#\#\#
New timeseries plot taking into account if the day is weekend or not

``` r
d <- mutate(newdf, Weekday = ifelse(Day >= 6, "Weekend", "Weekday"))

d <- d %>% group_by(Interval, Weekday) %>% summarise(Mean = mean(as.numeric(Steps)))
```

    ## `summarise()` has grouped output by 'Interval'. You can override using the `.groups` argument.

``` r
ggplot(d) + geom_line(aes(x = as.numeric(Interval), y = Mean, col= Weekday), size = 1) + ggtitle("Interval vs. Average steps") + xlab("Interval") + ylab("Average Steps")
```

![](CP1_files/figure-markdown_github/unnamed-chunk-11-1.png)
