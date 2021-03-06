``` r
library("knitr")
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'

    ## Die folgenden Objekte sind maskiert von 'package:stats':
    ## 
    ##     filter, lag

    ## Die folgenden Objekte sind maskiert von 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("ggplot2")
library("lattice")

setwd("/home/matthias/Dokumente/Matsche/Data_Science/RDR")


activity_raw <- read.csv("activity.csv")

activity_raw$date <- as.POSIXct(activity_raw$date, format="%Y-%m-%d")

activity_raw <- data.frame(date=activity_raw$date, 
                           weekday=tolower(weekdays(activity_raw$date)), 
                           steps=activity_raw$steps, 
                           interval=activity_raw$interval)

activity_raw <- cbind(activity_raw, 
                      daytype=ifelse(activity_raw$weekday == "saturday" | 
                                       activity_raw$weekday == "sunday", "weekend", 
                                     "weekday"))
                                     
activity <- data.frame(date=activity_raw$date, 
                       weekday=activity_raw$weekday, 
                       daytype=activity_raw$daytype, 
                       interval=activity_raw$interval,
                       steps=activity_raw$steps)
head(activity)
```

    ##         date weekday daytype interval steps
    ## 1 2012-10-01  montag weekday        0    NA
    ## 2 2012-10-01  montag weekday        5    NA
    ## 3 2012-10-01  montag weekday       10    NA
    ## 4 2012-10-01  montag weekday       15    NA
    ## 5 2012-10-01  montag weekday       20    NA
    ## 6 2012-10-01  montag weekday       25    NA

``` r
sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)


names(sum_data) <- c("date", "total")

head(sum_data)
```

    ##         date total
    ## 1 2012-10-01     0
    ## 2 2012-10-02   126
    ## 3 2012-10-03 11352
    ## 4 2012-10-04 12116
    ## 5 2012-10-05 13294
    ## 6 2012-10-06 15420

``` r
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 20), 
     main="Histogram of the total number of steps taken each day\n(NA removed)")
```

![](rdr_PGA_files/figure-markdown_github/unnamed-chunk-3-1.png?raw=true)

``` r
mean(sum_data$total)
```

    ## [1] 9354.23

``` r
median(sum_data$total)
```

    ## [1] 10395

``` r
rm(sum_data)
```

``` r
mean_data <- aggregate(activity$steps, 
                       by=list(activity$interval), 
                       FUN=mean, 
                       na.rm=TRUE)


names(mean_data) <- c("interval", "mean")

head(mean_data)
```

    ##   interval      mean
    ## 1        0 1.7169811
    ## 2        5 0.3396226
    ## 3       10 0.1320755
    ## 4       15 0.1509434
    ## 5       20 0.0754717
    ## 6       25 2.0943396

``` r
plot(mean_data$interval, 
     mean_data$mean, 
     type="l", 
     col="blue", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals\n(NA removed)")
```

![](rdr_PGA_files/figure-markdown_github/unnamed-chunk-6-1.png?raw=true)

``` r
max_pos <- which(mean_data$mean == max(mean_data$mean))


max_interval <- mean_data[max_pos, 1]


rm(max_pos, mean_data)


rm(max_interval)

NA_count <- sum(is.na(activity$steps))

rm(NA_count)

na_pos <- which(is.na(activity$steps))

mean_vec <- rep(mean(activity$steps, na.rm=TRUE), times=length(na_pos))

activity[na_pos, "steps"] <- mean_vec

rm(mean_vec, na_pos)

head(activity)
```

    ##         date weekday daytype interval   steps
    ## 1 2012-10-01  montag weekday        0 37.3826
    ## 2 2012-10-01  montag weekday        5 37.3826
    ## 3 2012-10-01  montag weekday       10 37.3826
    ## 4 2012-10-01  montag weekday       15 37.3826
    ## 5 2012-10-01  montag weekday       20 37.3826
    ## 6 2012-10-01  montag weekday       25 37.3826

``` r
sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum)
names(sum_data) <- c("date", "total")

hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day\n(NA replaced by mean value)")
```

![](rdr_PGA_files/figure-markdown_github/unnamed-chunk-8-1.png?raw=true)

``` r
mean(sum_data$total)
```

    ## [1] 10766.19

``` r
median(sum_data$total)
```

    ## [1] 10766.19

``` r
head(activity)
```

    ##         date weekday daytype interval   steps
    ## 1 2012-10-01  montag weekday        0 37.3826
    ## 2 2012-10-01  montag weekday        5 37.3826
    ## 3 2012-10-01  montag weekday       10 37.3826
    ## 4 2012-10-01  montag weekday       15 37.3826
    ## 5 2012-10-01  montag weekday       20 37.3826
    ## 6 2012-10-01  montag weekday       25 37.3826

``` r
rm(sum_data)


mean_data <- aggregate(activity$steps, 
                       by=list(activity$daytype, 
                               activity$weekday, activity$interval), mean)

names(mean_data) <- c("daytype", "weekday", "interval", "mean")

head(mean_data)
```

    ##   daytype    weekday interval     mean
    ## 1 weekday   dienstag        0 0.000000
    ## 2 weekday donnerstag        0 9.375844
    ## 3 weekday    freitag        0 8.307244
    ## 4 weekday   mittwoch        0 7.931400
    ## 5 weekday     montag        0 9.418355
    ## 6 weekday    samstag        0 4.672825

``` r
xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```

![](rdr_PGA_files/figure-markdown_github/unnamed-chunk-13-1.png?raw=true)

``` r
knit("rdr_PGA.md")
```

    ## 
    ## 
    ## processing file: rdr_PGA.md

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |.................................................................| 100%
    ##   ordinary text without R code

    ## output file: rdr_PGA.txt

    ## [1] "rdr_PGA.txt"
