unzip("activity.zip")
library(data.table)
actDat <- data.table(read.csv("activity.csv"))
actDat$date <- as.Date(actDat$date)

#remove NA
actDat <- na.omit(actDat)

sumSteps <- actDat[,list(sumSteps=sum(steps)),by=date]

#generate a histogram
hist(sumSteps$sumSteps,
     breaks=length(sumSteps$sumSteps),
     xlab="Quantity of Steps by Day",
     main="Steps By Day",
     col="blue")
meanStepsPerDay <- mean(sumSteps$sumSteps)
medianStepsPerDay <- median(sumSteps$sumSteps)

#average daily activity pattern
#time series
meanByInterval <- actDat[,list(meanSteps=mean(steps)),by=interval]
plot(meanByInterval$meanSteps~meanByInterval$interval,
     type="l",
     xlab="Interval",
     ylab="Average Daily Steps")




file.remove("activity.csv")