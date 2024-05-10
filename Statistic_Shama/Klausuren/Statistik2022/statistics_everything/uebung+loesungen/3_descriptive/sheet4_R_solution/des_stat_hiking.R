#####################################################
# Descriptive Statistics: Exercise 3.1, 
# Heumann, Schomaker, page 63
#
# File: des_stat_hiking.R
#
###################################################### 
# load tidyverse and xtable
library(tidyverse)
library(xtable)

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

# variance
var(altitude)
var(distance)

# coefficients of variation
sd(distance)/mean(distance)
sd(altitude)/mean(altitude)

# boxplots
par(mfrow=c(1,2))
boxplot(altitude,xlab="",ylab="Altitude (in m)", 
        main = "altitude",
        cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75) 
boxplot(distance,xlab="",ylab="Distance (in km)", 
        main = "distance",
        cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75) 
# eps-file
dev.copy2eps(file="../pictures/hiking_bp.eps")

# grouped data
bounds <- c(5,15,20,30)
dist_cut <- cut(distance, breaks = bounds)
                 
df_cut <- tibble(values = dist_cut)
df_cut_tab <-
  df_cut %>%
  group_by(values) %>%
  count() %>%
  mutate(rel = n / length(distance)) %>%
  ungroup() %>%
  mutate(cum_rel_freq = cumsum(rel))
df_cut_tab
# tex table
tex_tab <- xtable(df_cut_tab)
print(tex_tab, include.rownames = FALSE, floating = FALSE)

midpoints.classes <- (bounds[-1]+bounds[-4])/2
mean.grouped <- sum(midpoints.classes * df_cut_tab$rel)
mean.grouped
# weighted.mean(c(10,17.5,25),c(4/10,4/10,2/10))
