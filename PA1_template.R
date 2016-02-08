# Load Libraries
require(knitr) 
require(markdown)
require(dplyr)


setwd("~/ReproResearch/RepData_PeerAssessment1")

# Check if "data" directory exists
if(!dir.exists("./data")){
  dir.create("./data");
}


## URL of Dataset: This file contains a data frame with all of the PM2.5 emissions data
fileActivityZip <- "./activity.zip"

## Check the file exist or unzip
if(!file.exists("./data/activity.csv")){
  unzip(fileActivityZip, list = FALSE, overwrite = FALSE, exdir = "./data")
}

# Load data 
activity <- read.csv("./data/activity.csv") %>%
              transform(steps = as.numeric(steps)) %>%
                transform(date = as.Date(date,"%Y-%m-%d")) %>%
                  transform(interval = as.numeric(interval))

activities <- na.omit(activity) %>%
  group_by(date) %>% 
  summarize(total=sum(steps))


## What is mean total number of steps taken per day?

## For this part of the assignment, you can ignore the missing values in the dataset.

## - Make a histogram of the total number of steps taken each day.
## - Calculate and report the mean and median total number of steps taken per day.


# Create Graphic.
dev.new()
plot(activities$date,activities$total,type = "h", 
       main = "Histogram of total number of steps per day", 
        xlab = "Total number of steps in a day",
         ylab = "Steps", pch = 12, col = "blue", lty = 2)

# get mean and median total number of steps per day
mean(activities$total)

median(activities$total)



## What is the average daily activity pattern?

## - Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

## - Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


## Get the intervals by steps
activities_int <- na.omit(activity) %>%
  group_by(interval ) %>% 
  summarize(total=mean(steps)) ;

dev.new()
plot(activities_int$total,activities_int$steps,     type='l', col="blue", 
     main="Average number of steps averaged over all days", xlab="Interval", 
       ylab="Average number of steps")

# get the interval with maximum average number of steps in an interval
activities_int %>% top_n(n=1)

# number of rows with NA's
nrow(activity[!complete.cases(activity),]) - nrow(activities)

activy_na_rep <- activity %>%
                 mutate(steps =  replace(steps, which(is.na(steps)),mean(steps,na.rm=TRUE)))  %>% 
                   group_by(date) %>% 
                      summarize(total=sum(steps))



plot(activy_na_rep$total,activy_na_rep$steps,     type='h', col="blue", 
     main="Average number of steps averaged over all days", xlab="Interval", 
     ylab="Average number of steps")


library(chron)
#activy_na_rep

activity_days2 <-activity_days

activity_days <- na.omit(activity) 

activity_days[, "datetype"] <-     sapply(activity_days$date, function(x){if(!is.weekend(x))"weekenday" else "weekend"})
activity_days[, "weekday"] <-     sapply(activity_days$date, function(x){format(x, format="%A")})

activity_days2 <-activity_days
activity_days <- activity_days2

  activity_days <- activity_days %>%
                    group_by(date, datetype, weekday  ) %>%
    summarise_each(funs(mean))


activity_days2 <-activity_days  
  


head(activity_days)

myplot <- qplot(  interval  ,steps , data=activity_days, color=datetype)
myplot + geom_line() + facet_grid(datetype~ .) 


# Copy and save the generate image
dev.copy(png, file="./myplot.png", width=480, height=480)
dev.off()


