#####################################################
# Descriptive Statistics: Exercise 3.1, 
# Heumann, Schomaker, page 63
# Solution
#
# File: des_stat_hiking_sol.R
#
###################################################### 
library(tidyverse)

# generate the data
distance <- c(12.5,29.9,14.8,18.7,7.6,16.2,16.5,27.4,12.1,17.5)
altitude <- c(342,1245,502,555,398,670,796,912,238,466)
# sorted data
sort(distance)
sort(altitude)

# mean and median
mean(distance)
mean(altitude)

# R offers several ways of calculating quantiles. Use type=1 
# to apply the method we have introduced.
quantile(distance,probs = c(0.25,0.5,0.75),type=1)
quantile(altitude,probs = c(0.25,0.5,0.75),type=1)

# interquartial range
quantile(distance,probs=0.75,type=1)- quantile(distance,probs=0.25,type=1)
quantile(altitude,probs=0.75,type=1)- quantile(altitude,probs=0.25,type=1)

# shape of the distributions
# distance: Q2 closer to Q3, i.e. distance might be left skewed
# altitude: Q2 closer to Q1, i.e. distance might be right skewed
boxplot(altitude, main = "altitude", xlab="altitude")
boxplot(distance, main = "distance", xlab="distance")
boxplot(altitude, distance, 
        main = "altitude and distance",
        names = c("distance", "altitude"))

# variance and standard deviation
var(distance)
var(altitude)
sd(distance)
sd(altitude)

# make the values comparable
distance.norm <- distance/mean(distance)
altitude.norm <- altitude/mean(altitude)
boxplot(altitude.norm, distance.norm, 
        main = "altitude and distance - normed",
        names = c("distance.norm", "altitude.norm"))

# coefficients of variation
sd(distance)/mean(distance) # = sd(distance.norm)
sd(altitude)/mean(altitude) # = sd(altitude.norm)


