OG <- "dataset_with_add_features.csv"
data <- read.csv(OG, header = TRUE, sep =",", quote = "\"", dec = ".")

# check null values
print(colSums(is.na(data)))

# thresholds for features calculation
delta_angle <- 0.33 # angle for the change rate in radians
vel_tr <- 0.89 # speed in m / s  
vr_soglia <- 0.26 # speed change percentage

# TRUE if the current and previous row have the same label, plt and user
cond <- c(TRUE, (data$Id_user[-nrow(data)] == data$Id_user[-1]) & (data$Id_perc[-nrow(data)] == data$Id_perc[-1]) & (data$Label[-nrow(data)] == data$Label[-1]))

# I calculate the size of the new table
dim_array <- table(cond)["FALSE"]

# initialization conditions to speed up execution
cond_delta_angle <- c(FALSE, abs(data$angle[-nrow(data)] - data$angle[-1]) > delta_angle)
cond_vel <- data$vel < vel_tr
cond_vel_0 <- data$vel > 0
cond_alt_777 <- data$Altitude == -777
cond_vr <- c(FALSE, (abs(data$vel[-1] - data$vel[-nrow(data)]) / data$vel[-1]) > vr_soglia)

# initialization of temporary variables for each trajectory
distanceTotal_i <- 0
vel_max_i <- 0
timeTotal_i <- 0
n_hcr <- 0
n_sr <- 0
n_vr <- 0
vr <- 0
altitudeSum <- 0

# initial altitude value 
altitude_max <- -776
n_777 <- 0
n_points <- 0

# vector initialization of features
distanceTotal <- vector(mode="double", length=dim_array)
time_total <- vector(mode="double", length=dim_array)
vel_max <- vector(mode="double", length=dim_array)
vel_avg <- vector(mode="double", length=dim_array)

# vector initialization of labels
label <- vector(mode="character", length=dim_array)

# vector initialization ID_perc
Id_perc <- vector(mode="character", length=dim_array)

# vector ID_user initialization
Id_user <- vector(mode="character", length=dim_array)

# Time_start vector initialization representing the path start time stamp
Time_start <- vector(mode="character", length=dim_array)
Time_start[1] <- as.character(data$Date_Time[1])

# Time_end vector initialization representing the path end time stamp
Time_end <- vector(mode="character", length=dim_array)

# initialization of the route average altitude vector
altitudeAvg <- vector(mode="double", length=dim_array)

# vector initialization of the maximum altitude reached during the trip
altitudeMax <- vector(mode="double", length=dim_array)

# initialization latitude vector of the starting point
latitudeStart <- vector(mode="double", length=dim_array)
latitudeStart[1] <- data$Latitude[1]

# latitude vector initialization of the end point
latitudeEnd <- vector(mode="double", length=dim_array)

# initialization vector longitude of the starting point
longitudeStart <- vector(mode="double", length=dim_array)
longitudeStart[1] <- data$Longitude[1]

# longitude vector initialization of the end point
longitudeEnd <- vector(mode="double", length=dim_array)

# Heading change rate
hcr <- vector(mode="double", length=dim_array)

# stop rate
sr <- vector(mode="double", length=dim_array)

# velocity change rate
vcr <- vector(mode="double", length=dim_array)

# number of points with height at -777 of a path
n777 <- vector(mode="double", length=dim_array)

# n points of a route
npoints <- vector(mode="double", length=dim_array)

i <- 1
for(i_row in 2:nrow(data))
{
  if(i_row %% 10000 == 0)
  {
    print(i_row)
  }
  if(cond[i_row])
  {
    # the total distance is the sum of the partial distances
    distanceTotal_i <- distanceTotal_i + data$distance[i_row]
    timeTotal_i <- timeTotal_i + data$delta_time[i_row]
    if(data$vel[i_row] > vel_max_i){
      # maximum speed of the route
      vel_max_i <- data$vel[i_row]
    }
    if(cond_delta_angle[i_row])
    {
      # the number of times it changes direction
      n_hcr <- n_hcr + 1
    }
    if(cond_vel[i_row])
    {
      # the counter containing the number of points of the route whose speed is lower than the threshold is updated
      n_sr <- n_sr + 1
    }
    if(cond_vel_0[i_row])
    {
      #vr <- abs(dati$vel[i_row - 1] - dati$vel[i_row]) / dati$vel[i_row]
      if(cond_vr[i_row])
      {
        n_vr <- n_vr + 1
      }
    }
    if(cond_alt_777[i_row])
    {
      # the counter containing the number of points of the route whose altitude is not known
      n_777 <- n_777 + 1
    }
    else
    {
      # calculation of altitude parameters
      altitudeSum <- altitudeSum + data$Altitude[i_row]
      if(altitude_max < data$Altitude[i_row])
      {
        altitude_max <- data$Altitude[i_row]
      }
    }
    n_points <- n_points + 1
  }
  else
  {
    # writing the final parameters of a path in the respective vectors
    distanceTotal[i] <- distanceTotal_i
    time_total[i] <- timeTotal_i
    vel_max[i] <- vel_max_i
    vel_avg[i] <- distanceTotal_i/timeTotal_i
    hcr[i] <- n_hcr/distanceTotal_i
    sr[i] <- n_sr/distanceTotal_i
    vcr[i] <- n_vr/distanceTotal_i
    npoints[i] <- n_points
    n777[i] <- n_777
    altitudeAvg[i] <- altitudeSum/(n_points-n_777)
    altitudeMax[i] <- altitude_max
    latitudeEnd[i] <- data$Latitude[i_row-1]
    longitudeEnd[i] <- data$Longitude[i_row-1]
    label[i] <- as.character(data$Label[i_row-1])
    Id_perc[i] <- as.character(data$Id_perc[i_row-1])
    Id_user[i] <- as.character(data$Id_user[i_row-1])
    Time_end[i] <- as.character(data$Date_Time[i_row-1])
    
    # reset parameters
    distanceTotal_i <- 0
    vel_max_i <- 0
    timeTotal_i <- 0
    n_hcr <- 0
    n_sr <- 0
    n_vr <- 0
    # vr <- 0
    n_777 <- 0
    n_points <- 0
    altitudeSum <- 0
    altitude_max <- -776
    
    # initialization initial parameters for the new path
    longitudeStart[i+1] <- data$Longitude[i_row]
    latitudeStart[i+1] <- data$Latitude[i_row]
    Time_start[i+1] <- as.character(data$Date_Time[i_row])
    
    # counter update
    i <- i+1
  }
}
# writing parameters for the last path of the file within the respective vectors
distanceTotal[i] <- distanceTotal_i
time_total[i] <- timeTotal_i
vel_max[i] <- vel_max_i
vel_avg[i] <- distanceTotal_i/timeTotal_i
hcr[i] <- n_hcr/distanceTotal_i
sr[i] <- n_sr/distanceTotal_i
vcr[i] <- n_vr/distanceTotal_i
npoints[i] <- n_points
n777[i] <- n_777
altitudeAvg[i] <- altitudeSum/(n_points-n_777)
altitudeMax[i] <- altitude_max
latitudeEnd[i] <- data$Latitude[i_row]
longitudeEnd[i] <- data$Longitude[i_row]
label[i] <- as.character(data$Label[i_row])
Id_perc[i] <- as.character(data$Id_perc[i_row])
Id_user[i] <- as.character(data$Id_user[i_row])
Time_end[i] <- as.character(data$Date_Time[i_row])



# creation of the dataframe containing all the values
data_fin <- data.frame(
  Id_user = Id_user,
  Id_perc = Id_perc,
  label = label,
  longitudeStart = longitudeStart,
  latitudeStart = latitudeStart,
  latitudeEnd = latitudeEnd,
  longitudeEnd = longitudeEnd,
  TimeStart = Time_start,
  TimeEnd = Time_end,
  StartDay = weekdays(as.Date(Time_start)),
  EndDay = weekdays(as.Date(Time_end)),
  StartHour = format(as.POSIXct(Time_start), format = "%H"),
  EndHour = format(as.POSIXct(Time_end), format = "%H"),
  distanceTotal = distanceTotal,
  time_total = time_total,
  n777 = n777,
  npoints = npoints,
  vel_avg = vel_avg,
  vel_max = vel_max,
  altitudeAvg = altitudeAvg * 0.3048, # transformation from feet to meters
  altitudeMax = altitudeMax * 0.3048, # transformation from feet to meters
  vcr = vcr,
  sr = sr,
  hcr = hcr
)

# Separate datetime
tstart_parts <- t(as.data.frame(strsplit(Time_start,' ')))
tend_parts <- t(as.data.frame(strsplit(Time_end,' ')))
row.names(tstart_parts) = NULL
row.names(tend_parts) = NULL
start_hour <- chron(times=tstart_parts[,2], format='h:m:s')

# writing data inside the csv file
write.csv(data_fin,file="compressed_dataset2.csv" ,row.names=FALSE)
