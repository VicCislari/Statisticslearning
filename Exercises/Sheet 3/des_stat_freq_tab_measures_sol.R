#####################################################
# Descriptive Statistics: Arithmetic mean, the geometric 
# mean, the harmonic mean and the trimmed 20% mean from 
# a frequency table
# Solution
#
# File: des_stat_freq_tab_measures_sol.R
#
#####################################################

library(tidyverse)

# Consider the freuency table
f.tab <- tibble(
  obs = 1:8,
  n = c(5,4,1,7,2,3,1,2)
)
f.tab
# convert to an ordered sample
x.ordered <- rep(f.tab$obs,f.tab$n)

# a) mean
mean(x.ordered)

# b) geometric mean
(f.tab$obs**f.tab$n %>% prod())**(1/sum(f.tab$n))

# c) harmonic mean
sum(f.tab$n)/sum(f.tab$n/f.tab$obs)

# d) trimmed 20% mean
# mean(x, trim = 0, na.rm = FALSE, ...)
# x	R object
# trim the fraction (0 to 0.5) of observations to be trimmed from each end of x 
# before the mean is computed. Values of trim outside that range are taken as 
# the nearest endpoint.
mean(x.ordered,trim = 0.1)
