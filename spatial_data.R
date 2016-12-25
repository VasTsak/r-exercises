library(dplyr)
airports <- read.csv('airports.csv')
flights <- tbl_df(flights) 
airports <- tbl_df(airports) 
# we will use only the rows without NA's 
flights_NA <-flights[which(!is.na(flights['WeatherDelay'])),]
airports <- airports[c("iata","lat","long")]

dest <- merge.data.frame(flights_NA, airports, by.x = "Dest", by.y = "iata")
colnames(dest)[which(names(dest) == "lat")] <- "d_lat"
colnames(dest)[which(names(dest) == "long")] <- "d_long"

origin <- merge.data.frame(flights_NA, airports, by.x = "Origin", by.y = "iata")
colnames(origin)[which(names(origin) == "lat")] <- "o_lat"
colnames(origin)[which(names(origin) == "long")] <- "o_long"

dest <- dest[order(dest$Dest,dest$DepTime),]
origin <- origin[order(origin$Dest,origin$DepTime),] 

head(dest)
head(origin)

flights <- dest
flights[c("o_lat","o_long")] <- origin[,c("o_lat","o_long")]
