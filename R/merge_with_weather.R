---
title: "full_geolife+weather"
author: "Ekin Ugurel"
date: "2/27/2022"
output: html_document
---
  
setwd("C:/Users/ekino/Downloads/")
toy1 <- read.csv("New_compressed_dataset3.csv", header=TRUE)

df <- data.frame(toy1)

hi <- t(as.data.frame(strsplit(df$Converted_TimeStart, " ")))
row.names(hi) = NULL
date <- format(as.Date(hi[,1], '%m/%d/%Y'), "%Y-%m-%d")
df$date <- date

weather <- read.csv("Beijing,China Weather Data.csv", header=TRUE)
weather_df <- data.frame(weather)
weather_df <- weather_df[,2:5,9:11,14:19,21:22, 25:28]
names(weather_df)[2] <- "date"
weather_df$date <- format(as.Date(weather_df$date, '%d-%m-%Y'), "%Y-%m-%d")

a <- merge(x = df, y = weather_df, by="date")

write.csv(a, file="full_geolife+weather.csv", row.names=FALSE)
