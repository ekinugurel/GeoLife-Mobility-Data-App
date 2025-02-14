setwd("C:/Users/ekino/Downloads/")
toy1 <- read.csv("full_geolife+weather.csv", header=TRUE)

walk <- subset(data.frame(toy1), (toy1$time_total < 18000) & (toy1$time_total > 150))

walk <- subset(walk, walk$label == 'walk')

# TO-DO: group factors (e.g. weekdays/weekends, workhours/offhours, )
modes <- as.factor(walk$label)

# num_modes <- recode(df$label, "walk" = 0, "bike" = 1, "bus" = 2, "subway" = 3, "taxi" = 4, "car" = 5)

date <- walk$date
day <- as.factor(walk$Converted_Start_Day)
start_hour <- as.factor(walk$Converted_Start_Hour)
end_hour <- as.factor(walk$Converted_End_Hour)
avg_vel <- walk$vel_avg
total_dist <- walk$distanceTotal
total_time <- as.numeric(walk$time_total)
temp <- walk$temp
rain <- walk$precip
windspeed <- walk$windspeed
humidity <- walk$humidity
cloudcover <- walk$cloudcover
# conditions <- as.factor(df$conditions)
hcr <- walk$hcr # heading change rate: ratio btwn number of GPS points where a user changes direction with an angle > threshold 
#and the total distance of the route
vcr <- walk$vcr # velocity change rate: ratio btwn the number of GPS points with speed variation
# above a certain threshold per unit of distance and the total distance of the route
npoints <- walk$npoints # do not group walk/bike with this
sr <- walk$sr # stop rate: ratio between the number of GPS points below a certain speed threshold per unit of distance and the total 
# distance of the route... do not group walk/bike

levels(day) <- c(1, 1, 0, 0, 1, 1, 1) # 1 = weekday, 0 = weekend
#levels(start_hour) <- c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,1,1,1,1,1,0,0,0,0,0) # 0 = off, 1 = work
levels(start_hour) <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0,0,0,1,1,1,1,0,0,0,0) # peak hours
levels(end_hour) <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0,0,0,1,1,1,1,1,1,0,0) # peak hours

#levels(end_hour) <- c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1,1,1,1,1,1,0,0,0,0,0)

full <- data.frame(day, start_hour, end_hour, avg_vel, total_dist, total_time, temp, rain, windspeed, humidity, cloudcover, hcr, vcr, npoints, sr)

full_alt <- data.frame(avg_vel, total_dist, total_time, hcr, vcr, npoints, sr)

X1 <- data.frame(start_hour, end_hour)

X2 <- data.frame(avg_vel, total_dist, total_time, hcr, vcr, npoints, sr, day, start_hour, end_hour) 

X3 <- data.frame(temp, rain, windspeed, humidity, cloudcover)
# X3 <- data.frame(temp)
# rain <- data.frame(rain, humidity, cloudcover)

X1_k <- kmeans(X1, center=2) # time
X2_k <- kmeans(X2, center=3) # metrics
X3_k <- kmeans(X3, center=2) # weather

X1_k$centers
X2_k$centers
X3_k$centers

library(dplyr)
timeC<- full %>% mutate(X1_k$cluster) # 1: 
metricC <- full %>% mutate(X2_k$cluster) # 1: walkers with a purpose -- 2: strollers
weatherC <- full %>% mutate(X3_k$cluster) # 1: warm -- 2: cold

rainy_walk <- subset(weatherC, weatherC$`X3_k$cluster` == 1)[,]

# Average __ in warm vs cold weather
mean(subset(weatherC, weatherC$`X3_k$cluster` == 1)[,15]) # stop rate
mean(subset(weatherC, weatherC$`X3_k$cluster` == 2)[,15]) # stop rate

# Average __ in long distance vs. short distance drives
mean(subset(weatherC, metricC$`X3_k$cluster` == 1)[,15]) # stop rate
mean(subset(weatherC, metricC$`X3_k$cluster` == 2)[,15]) # stop rate

library(ggplot2)
ggplot(weatherC, aes(x=as.factor(weatherC$`X3_k$cluster`), y=weatherC$sr, fill=as.factor(weatherC$`X3_k$cluster`))) +
  geom_boxplot() + ylim(0,0.25) #+ theme(legend.title = 'Weather')#, legend.text = c("Warm", "Cold"))

table(subset(metricC, metricC$`X2_k$cluster` == 1)[,1])
table(subset(metricC, metricC$`X2_k$cluster` == 2)[,1])


# library(cluster)
# clusplot(full, X1_k$cluster, color=TRUE, shade=TRUE, labels=1, lines=0)

pairs(weatherC, main = "Favorableness towards Seattle", pch = 21, bg = c("red", "green3")[unclass(X3_k$cluster)])
par(xpd=TRUE)
legend(1,1, as.vector(unique(X3_k$cluster)), fill=c("red","green3"))

par(mfrow=c(1,1))
Group1<-X1_k$centers[1,]
Group2<-X1_k$centers[2,]
#Group3<-vars_kmeans$centers[3,] # comment this out to see two center cluster
plot(Group1,Group2, type="n",xlab="Group 1",ylab="Group2")
text(Group1,Group2, labels=c("day", "start hour", "end hour"))

library(factoextra)
fviz_nbclust(timeC, kmeans, method="silhouette", k.max = 10) + theme_minimal() + ggtitle("The Silhouette Plot")
fviz_nbclust(metricC, kmeans, method="silhouette", k.max = 10) + theme_minimal() + ggtitle("The Silhouette Plot")
fviz_nbclust(weatherC, kmeans, method="silhouette", k.max = 10) + theme_minimal()

