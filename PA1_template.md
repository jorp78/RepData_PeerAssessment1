# Reproducible Research: Peer Assessment 1
Jonattan Ramirez  
Sunday, February 07, 2016  
require(knitr)
require(markdown)

knit2html("PA1_template.Rmd", encoding="ISO8859-1")  
markdownToHTML("PA1_template.md", "PA1_template.html")

## Loading and preprocessing the data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
fileActivityZip <- "./activity.zip"

if(!file.exists("./data/activity.csv")){
  unzip(fileActivityZip, list = FALSE, overwrite = FALSE, exdir = "./data")
}

activity <- read.csv("./data/activity.csv") %>%
              transform(steps = as.numeric(steps)) %>%
                transform(date = as.Date(date,"%Y-%m-%d")) %>%
                  transform(interval = as.numeric(interval))
```

## What is mean total number of steps taken per day?

```r
activities <- na.omit(activity) %>%
  group_by(date) %>% 
  summarize(total=sum(steps))

#### Create Histogram.
dev.new()
plot(activities$date,activities$total,type = "h", 
       main = "Histogram of total number of steps per day", 
        xlab = "Total number of steps in a day",
         ylab = "Steps", pch = 12, col = "blue", lty = 2)

#### Copy and save the generate image
dev.copy(png, file="./Hist_total_steps_day.png", width=480, height=480)
```

```
## png 
##   4
```

```r
dev.off()
```

```
## png 
##   2
```

## What is the average daily activity pattern?

```r
#### get mean and median total number of steps per day
mean(activities$total)
```

```
## [1] 10766.19
```

```r
median(activities$total)
```

```
## [1] 10765
```

```r
#### Get the intervals by steps
activities_int <- na.omit(activity) %>%
  group_by(interval ) %>% 
  summarize(total=mean(steps)) ;
  
#### Create Graphic.
dev.new()
plot(activities_int$total, activities_int$steps,     type='l', col="blue", 
     main="Average number of steps averaged over all days", xlab="Interval", 
       ylab="Average number of steps")

#### Copy and save the generate image
dev.copy(png, file="./Average_steps_days.png", width=480, height=480)
```

```
## png 
##   5
```

```r
dev.off()
```

```
## png 
##   2
```

## Imputing missing values

```r
#### get the interval with maximum average number of steps in an interval
activities_int %>% top_n(n=1)
```

```
## Selecting by total
```

```
## Source: local data frame [1 x 2]
## 
##   interval    total
##      (dbl)    (dbl)
## 1      835 206.1698
```

```r
#### number of rows with NA's
nrow(activity[!complete.cases(activity),]) - nrow(activities)
```

```
## [1] 2251
```

```r
activy_na_rep <- activity %>%
  mutate(steps =  replace(steps, which(is.na(steps)),mean(steps,na.rm=TRUE)))  %>% 
       group_by(date) %>% 
           summarize(total=sum(steps))


#### Create Graphic.
plot(activy_na_rep$total,activy_na_rep$steps,type='h', col="blue", 
     main="Average number of steps averaged over all days with NA's", xlab="Interval", 
     ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

```r
#### Copy and save the generate image
dev.copy(png, file="./Average_with_NAs.png", width=480, height=480)
```

```
## png 
##   5
```

```r
dev.off()
```

```
## png 
##   2
```



## Are there differences in activity patterns between weekdays and weekends?

```r
library(chron)
library(ggplot2)
activity_days <- na.omit(activity) 

activity_days[, "datetype"] <-    
    sapply(activity_days$date,  function(x){if(!is.weekend(x))"weekenday" else "weekend"})
activity_days[, "weekday"] <-
           sapply(activity_days$date, function(x){format(x, format="%A")})

  activity_days <- activity_days %>%
    group_by(date, datetype, weekday, interval) %>% 
      summarise_each(funs(mean))



#### Create Graphic.
myplot <- qplot(  interval  ,steps , data=activity_days, color=datetype)
myplot + geom_line() + facet_grid(datetype~ .) 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)

```r
#### Copy and save the generate image
dev.copy(png, file="./myplot.png", width=480, height=480)
```

```
## png 
##   5
```

```r
dev.off()
```

```
## png 
##   2
```
