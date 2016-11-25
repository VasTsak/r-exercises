flights <- read.csv('2008.csv')
library(stringr)
library(chron)
library(Hmisc)
library(dplyr)

flights <- tbl_df(flights) 

dep_time <- str_pad(flights$DepTime, 4, pad = "0")
hour <- substring(dep_time,1,2)
minutes <- substring(dep_time,3,4)
for(i in 1:length(dep_time)){
  dep_time[i] <- paste(c(hour[i],minutes[i],'00'),collapse = ':')
}
dep_time <- chron(times=dep_time)
flights$DepTime<-dep_time

arr_time <- str_pad(flights$ArrTime, 4, pad = "0")
hour <- substring(arr_time,1,2)
minutes <- substring(arr_time,3,4)
for(i in 1:length(arr_time)){
  arr_time[i] <- paste(c(hour[i],minutes[i],'00'),collapse = ':')
}
arr_time <- chron(times=arr_time)
flights$ArrTime<-arr_time

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

flights$ArrTime[which(is.na(flights$ArrTime))] <- mean(flights$ArrTime) 

flights$CRSArrTime[which(is.na(flights$CRSArrTime))] <- median(flights$CRSArrTime, na.rm = TRUE) 

flights <- transform(flights, DepTime = chron(times=ifelse(is.na(DepTime), mean(DepTime, na.rm=TRUE), DepTime)))
flights <- transform(flights, AirTime = ifelse(is.na(AirTime), mean(AirTime, na.rm=TRUE), AirTime))


flights$ArrDelay <- impute(flights$ArrDelay ,mean)

flights$CRSElapsedTime <- impute(flights$CRSElapsedTime,median)

lm_dep_delay <- lm (DepDelay ~ ArrTime+AirTime+ArrDelay +DepTime,data=flights)

pred_dep_delay <- predict (lm_dep_delay, flights)

impute_pred <- function (a, a_impute){
  ifelse (is.na(a), a_impute, a)
}

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

#1 
flights%>%
select(Dest,ArrDelay,AirTime)

#2
flights%>%select(contains('Delay'))

#3
flights %>% filter(CarrierDelay>180) %>%select(UniqueCarrier,DayOfWeek,Month)

#4
flights %>% group_by(UniqueCarrier)

#5 summarise: this function performs summary statistics 
flights %>% summarise(Mean_AirDelay = mean(AirTime))

#6 

flights %>% summarise(Min = min(AirTime),
                      Mean = mean(AirTime),
                      Median = median(AirTime),
                      Var = var(AirTime),
                      Std = sd(AirTime),
                      Max = max(AirTime),
                      Count = n())

#7
flights %>%
  group_by(UniqueCarrier)%>% 
  summarise(Mean_Delay = mean(ArrDelay),
            Number_of_flights = n())

#8
flights %>%
  group_by(UniqueCarrier)%>% 
  summarise(Mean_Delay = mean(ArrDelay, na.rm=TRUE),
            Number_of_flights = n()) %>%
  arrange(desc(Mean_Delay))

#9
flights %>% group_by(UniqueCarrier)%>%
  summarise(Number_of_flights = n(),
            Mean_Cancelled = mean(Cancelled)
            )%>%
  arrange(desc(Mean_Cancelled))


# 10

flights$Year <- as.character(flights$Year)
flights$Month <- as.character(flights$Month)
flights$DayofMonth <- as.character(flights$DayofMonth)

flights <- flights %>% mutate(Full_Date = chron(date=paste(Month,DayofMonth,Year,sep='/')))
flights %>% select(Full_Date)

flights %>% group_by(Full_Date)%>% 
summarise(Number_of_flights = n())%>%
arrange(desc(Number_of_flights))

flights %>% group_by(Full_Date)%>%
summarise(Number_of_flights = n(),Ratio_of_Cancelled = mean(Cancelled))%>%
arrange(desc(Ratio_of_Cancelled))%>%
select(Full_Date,Number_of_flights,Ratio_of_Cancelled)

flights %>%
group_by(Full_Date)%>%
summarise(Mean_ArrDelay= mean(ArrDelay))

# if you wish to save the dataset for the other exercises to come, run the command below
#write.csv(flights,'flights_2008.csv')
