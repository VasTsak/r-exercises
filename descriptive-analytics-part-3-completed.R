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
