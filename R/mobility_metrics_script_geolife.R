rm(list=ls())
library(tidyverse)
deg2rad <- function(deg) {(deg * pi) / (180)}
if(!require(geosphere)){
  install.packages("geosphere")
  library("geosphere")
}
# Import dataset
setwd("C:/Users/ekino/Downloads")
OG <- "dataset_raw_full.csv"
data <- read.csv(OG, header = TRUE, sep =",", quote = "\"", dec = ".")

######1 
# Delete lines which have latitude > 90 0 <-90
data <- data[data$Latitude < 90,]
data <- data[data$Latitude > -90,]

# Delete lines which have latitude > 180 0 <-180
data <- data[data$Longitude < 180,]
data <- data[data$Longitude > -180,]

cond <- c(FALSE, (data$Id_user[-nrow(data)] == data$Id_user[-1]) & (data$Id_perc[-nrow(data)] == data$Id_perc[-1]) & (data$Label[-nrow(data)] == data$Label[-1]))

longitude <- data$Longitude
latitude <- data$Latitude
date_time <- data$Date_Time

# new feature
data$distance <- 0
data$vel <- 0
data$delta_time <- 0
data$angle <- 0

distance <- data$distance
vel <- data$vel
delta_time <- data$delta_time
angle <- data$angle

for(i_row in 1:nrow(data))
{
  if(i_row %% 10000 == 0)
  {
    # print for debug
    print(i_row)
  }
  if(cond[i_row])
  {
    # if the previous line and the current line have id_user and id_perc and label the same
    # calculate the distance in meters between the previous point and the current point
    distance[i_row] <- distGeo(c(longitude[i_row-1], latitude[i_row-1]), c(longitude[i_row], latitude[i_row]))
    # calculate in seconds the time between the previous point and the next
    delta_time[i_row] <- as.numeric(difftime(date_time[i_row], date_time[i_row-1], units = "secs"))
    # if the delta time or delta distance are at 0 set the speed to 0
    if(distance[i_row] == 0 | delta_time[i_row] == 0)
    {
      vel[i_row] = 0
    }
    else
    {
      # calculate speed
      vel[i_row] <- distance[i_row]/delta_time[i_row]
    }
    # calculate the angle between north and two coordinates
    bearing <- atan2(sin(deg2rad(longitude[i_row]) - deg2rad(longitude[i_row-1])) * cos(deg2rad(latitude[i_row])),
                     cos(deg2rad(latitude[i_row-1])) * sin(deg2rad(latitude[i_row])) - sin(deg2rad(latitude[i_row-1]))
                     * cos(deg2rad(latitude[i_row]))
                     * cos(deg2rad(longitude[i_row]) - deg2rad(longitude[i_row-1])))
    bearing = bearing + 2.0 * pi
    while(bearing > 2.0 * pi)
    {
      bearing = bearing - 2.0 * pi
    }
    angle[i_row] <- bearing
  }
}

data$distance <- distance
data$vel <- vel
data$delta_time <- delta_time
data$angle <- angle
# save the new CSV
write.csv(data,file="dataset_with_add_features.csv" ,row.names=FALSE) 
