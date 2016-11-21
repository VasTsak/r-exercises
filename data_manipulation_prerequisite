# Setting working directory
# Download the '2008' file from 'http://stat-computing.org/dataexpo/2009/the-data.html'
# Change the file format to csv
setwd('')
flights <- read.csv('2008.csv')

summary(flights[name])
library(stringr)
library(chron)

dep_time <- str_pad(flights$DepTime, 4, pad = "0")
hour <- substring(dep_time,1,2)
minutes <- substring(dep_time,3,4)
for(i in 1:length(dep_time)){
  dep_time[i] <- paste(c(hour[i],minutes[i],'00'),collapse = ':')
}
dep_time <- chron(times=dep_time)
flights$DepTime<-dep_time
head(flights$DepTime , n=10)

arr_time <- str_pad(flights$ArrTime, 4, pad = "0")
hour <- substring(arr_time,1,2)
minutes <- substring(arr_time,3,4)
for(i in 1:length(arr_time)){
  arr_time[i] <- paste(c(hour[i],minutes[i],'00'),collapse = ':')
}
arr_time <- chron(times=arr_time)
flights$ArrTime<-arr_time
head(arr_time)

crs_dep_time <- str_pad(flights$CRSDepTime, 4, pad = "0")
hour <- substring(crs_dep_time,1,2)
minutes <- substring(crs_dep_time,3,4)
for(i in 1:length(crs_dep_time)){
  crs_dep_time[i] <- paste(c(hour[i],minutes[i],'00'),collapse = ':')
}
crs_dep_time <- chron(times=crs_dep_time)
flights$CRSDepTime<-crs_dep_time


crs_arr_time <- str_pad(flights$CRSArrTime, 4, pad = "0")
hour <- substring(crs_arr_time,1,2)
minutes <- substring(crs_arr_time,3,4)
for(i in 1:length(crs_arr_time)){
  crs_arr_time[i] <- paste(c(hour[i],minutes[i],'00'),collapse = ':')
}
crs_arr_time <- chron(times=crs_arr_time)
flights$CRSArrTime<-crs_arr_time

summary(flights)

flights$ArrTime[which(is.na(flights$ArrTime))] <- mean(flights$ArrTime, na.rm = TRUE) 
flights$CRSArrTime[which(is.na(flights$CRSArrTime))] <- median(flights$CRSArrTime, na.rm = TRUE) 

head(dep_time)

flights <- transform(flights, DepTime = chron(times=ifelse(is.na(DepTime), mean(DepTime, na.rm=TRUE), DepTime)))
flights <- transform(flights, AirTime = ifelse(is.na(AirTime), mean(AirTime, na.rm=TRUE), AirTime))


library(Hmisc)
flights$ArrDelay <- impute(flights$ArrDelay ,mean)
flights$CRSElapsedTime <- impute(flights$CRSElapsedTime,median)

impute_pred <- function (a, a_impute){
  ifelse (is.na(a), a_impute, a)
}

lm_dep_delay <- lm (DepDelay ~ ArrTime+AirTime+ArrDelay +DepTime,data=flights)
pred_dep_delay <- predict (lm_dep_delay, flights)
flights$DepDelay <- impute_pred (flights$DepDelay, pred_dep_delay) 

lm_actual_elapsed_time <- lm (ActualElapsedTime ~ArrTime+AirTime+ArrDelay +DepTime+DepDelay,data=flights)
pred_actual_elapsed_time <- predict (lm_actual_elapsed_time, flights)
flights$ActualElapsedTime <- impute_pred (flights$ActualElapsedTime, pred_actual_elapsed_time) 

lm_taxi_in <- lm (TaxiIn ~ArrTime+AirTime+ArrDelay +DepTime+DepDelay,data=flights)
pred_taxi_in <- predict (lm_taxi_in, flights)
flights$TaxiIn <- impute_pred (flights$TaxiIn, pred_taxi_in) 

lm_taxi_out <- lm (TaxiOut ~ArrTime+AirTime+ArrDelay +DepTime+DepDelay,data=flights)
pred_taxi_out <- predict (lm_taxi_out, flights)
flights$TaxiOut <- impute_pred (flights$TaxiOut, pred_taxi_out) 

summary(flights)

library(outliers)
library(rapportools)

flights <- subset(flights, !UniqueCarrier =='AQ')

flights <- subset(flights, !CancellationCode =='D')

flights <- flights[which(!(flights$ActualElapsedTime %in% boxplot.stats(flights$ActualElapsedTime)$out)),]

flights <- subset(flights, TaxiIn < 120 & TaxiIn > 0)

flights <- subset(flights, TaxiOut < 50 & TaxiOut >0)

flights$ArrDelay <- ifelse(flights$ArrDelay==outlier(flights$ArrDelay),NA,flights$ArrDelay)

rp.outlier(flights$Distance)

flights <- subset(flights, Distance != rp.outlier(flights$Distance))

write.csv(flights,'flights_2008.csv')

