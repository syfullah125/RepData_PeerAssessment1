``` r
knitr::opts_chunk$set(echo = TRUE)
```

\#\#Load Data

Firstly,I want to open the given dataset into a variable called
**MonitorData**.  
And want to see the head of the dataset.

``` r
MonitorData<- read.csv("activity.csv")
head(MonitorData)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

\#\#Plotting Histogram

Now let,s factorize the **date** variable and count the total steps in a
day.  
And we will create a new dataframe named **TotalStep** which will keep
these data And we will have a histogram of total steps per day

``` r
MonitorData$date<- as.factor(MonitorData$date)
x1<- data.frame(sapply(split(MonitorData,MonitorData$date),function(x){sum(x[,1],na.rm = TRUE)}))
TotalStep<- data.frame(Date=row.names(x1),TotalStep=x1[,1])
library(ggplot2)
        g <- ggplot(TotalStep, aes(x= Date, y = TotalStep))
        g + geom_bar(stat = 'identity', na.rm = T) +
           ggtitle("Total steps per day") +
           ylab("Steps per day")
```

![](coursera1_files/figure-markdown_github/hist-1.png) \#\#Mean and
Median of steps

Now I’m going to calculate the mean and median of Total Steps per day

``` r
data.frame(Mean=mean(TotalStep$TotalStep),Median=median(TotalStep$TotalStep),row.names = NULL)
```

    ##      Mean Median
    ## 1 9354.23  10395

\#\#Time series plotting

Now I will make a data frame named Average Step consisting the columns
of Interval and Average Step.  
For that I will factorize *interval* column and then make it.  
And going to make a plot of time series.

``` r
MonitorData$interval<- as.factor(MonitorData$interval)
x2<- data.frame(sapply(split(MonitorData,MonitorData$interval),function(x){mean(x[,1],na.rm = TRUE)}))
AverageStep<- data.frame(Interval=row.names(x2),AverageStep=x2[,1])
plot(AverageStep$Interval,AverageStep$AverageStep,main = "Time Series of average steps",xlab = "Interval",ylab="Average Step",type = "l")
```

![](coursera1_files/figure-markdown_github/timeseriesplot1-1.png)

\#\#Maximum Step

Now I’m going to calculate The 5-minute interval that, on average,
contains the maximum number of steps.

``` r
max(AverageStep$AverageStep,na.rm = TRUE)
```

    ## [1] 206.1698

\#\#Imputing NA Values

Now I am going to impute all the rows having **NA** values.And going to
make a new **NA free Dataframe**. Strategy:

-   Now firstly I’m going to make a intermediate variable which will be
    a logical  
    vector having no NA

-   Then will subset the dataframe.

``` r
x3<- !is.na(MonitorData$steps)
MonitorDatawithoutNA<- MonitorData[x3,]
```

\#\#Histogram with imputed dataset

Now I am going to draw a histogram with the imputed dataset.

``` r
MonitorDatawithoutNA$date<- as.factor(MonitorDatawithoutNA$date)
x4<- data.frame(sapply(split(MonitorDatawithoutNA,MonitorDatawithoutNA$date),function(x){sum(x[,1],na.rm = TRUE)}))
TotalStep2<- data.frame(Date=row.names(x4),TotalStep=x4[,1])
library(ggplot2)
        g2 <- ggplot(TotalStep2, aes(x= Date, y = TotalStep))
        g2 + geom_bar(stat = 'identity', na.rm = T) +
           ggtitle("Total steps per day") +
           ylab("Steps per day")
```

![](coursera1_files/figure-markdown_github/hist2-1.png)

\#\#Mean Median of iputed dataset The histogram doesn’t differ.Now let’s
compare the mean and median.

``` r
data.frame(Mean=mean(TotalStep2$TotalStep),Median=median(TotalStep2$TotalStep),row.names = NULL)
```

    ##      Mean Median
    ## 1 9354.23  10395

It doesn’t differ either.

\#\#Adding Variable

Now I,m going to add a new variable in dataset which will contain a
variable if it is a weekdays or not.

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
MonitorDatawithoutNA$date<- ymd(MonitorDatawithoutNA$date)
MonitorDatawithoutNA$status<- wday(MonitorDatawithoutNA$date,label = FALSE)
MonitorDatawithoutNA$status<- MonitorDatawithoutNA$status %in% c(1,2)
MonitorDatawithoutNA$status<- sub("FALSE","WEEKDAYS",MonitorDatawithoutNA$status)
MonitorDatawithoutNA$status<- sub("TRUE","WEEKEND",MonitorDatawithoutNA$status)
```

Firstly I will make a dataset with averagestep.

Then I am going to draw 2 plots containing a time series plot  
of the 5-minute interval (x-axis) and the average number of steps taken,
averaged across all weekday days or weekend days (y-axis).

``` r
MonitorDatawithoutNA$interval<- as.factor(MonitorDatawithoutNA$interval)
x5<- data.frame(sapply(split(MonitorDatawithoutNA,MonitorDatawithoutNA$interval),function(x){mean(x[,1],na.rm = TRUE)}))
AverageStep2<- data.frame(Interval=row.names(x5),AverageStep=x5[,1],status=MonitorDatawithoutNA$status)
AverageStep2<- split(AverageStep2,AverageStep2$status)
par(mfrow=c(1,2),mar=c(3,3,2,2),oma=c(1,1,1,1))
plot(AverageStep2$WEEKDAYS$Interval,AverageStep2$WEEKDAYS$AverageStep,xlab = "Interval",ylab = "Average Step",main = "Time series plotting in weekdays",type = "l")
plot(AverageStep2$WEEKEND$Interval,AverageStep2$WEEKEND$AverageStep,xlab = "Interval",ylab = "Average Step",main = "Time series plotting in weekend",type = "l")
mtext("Panel plot of time series ",outer=TRUE)
```

![](coursera1_files/figure-markdown_github/panel-1.png)
