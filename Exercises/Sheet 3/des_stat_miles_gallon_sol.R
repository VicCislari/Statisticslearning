#####################################################
# Descriptive Statistics: miles per gallon
# Solution
#
# File: des_stat_miles_gallon_sol.R
#
######################################################
library(tidyverse)

# inspect the description of the data set
?mpg()

# select only the variables displ and hwy and add 
# a colummn displ_class which denotes the belonging
# to one of the groups 
# low (1 < displ <= 3), medium (3 < displ <= 5),
# big (5 < displ <= 8)
tab <-
  mpg %>%
  select(displ,hwy) %>%
  mutate(displ_class = 
           cut(displ,breaks = c(1,3,5,8),
               labels = c("small","medium","big"))
  )
tab

# calculate mean, min, max Q1, Q2 and Q3 of the variable
# hwy grouped by the values of displ.
stat_hwy_displ <-
  tab %>%
  group_by(displ) %>%
  summarise(mean=mean(hwy),min=min(hwy),max=max(hwy),
            q1=quantile(hwy,0.25, type=1),
            q2=quantile(hwy,0.5, type=1),
            q3=quantile(hwy,0.75, type=1),
            nobs = n()
            )
stat_hwy_displ

# calculate mean, min, max Q1, Q2 and Q3 of the variable
# hwy grouped by the values of displ_class.
stat_hwy_class <-
  tab %>%
  group_by(displ_class) %>%
  summarise(mean(hwy),min(hwy),max(hwy),
            q1=quantile(hwy,0.25, type=2),
            q2=quantile(hwy,0.5, type=2),
            q3=quantile(hwy,0.75, type=2),
            nobs = n()
            )
stat_hwy_class

# boxplots of hwy grouped by displ
boxplot(hwy ~ displ, data = tab, xlab = "displ", ylab = "hwy")
# using ggplot
ggplot(data = tab) +
  geom_boxplot(mapping = aes(group = displ, x = displ, y = hwy)) + 
  theme_bw()

# boxplots of hwy grouped by displ_class
boxplot(hwy ~ displ_class, data = tab, xlab = "displ_class", ylab = "hwy")
# using ggplot
ggplot(data = tab) +
  geom_boxplot(mapping = aes(group = displ_class, x = displ_class, y = hwy)) +
#  geom_point(mapping = aes(group = displ_class, x = displ_class, y = hwy)) +
  theme_bw()

# both boxplots together
par(mfrow = c(1,2))
boxplot(hwy ~ displ, data = tab, xlab = "displ", ylab = "hwy")
boxplot(hwy ~ displ_class, data = tab, xlab = "displ_class", ylab = "hwy")
# zuruecksetzen von mfrow
par(mfrow = c(1,1))