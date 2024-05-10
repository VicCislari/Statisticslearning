#####################################################
# Descriptive Statistics: Different measure of sample
# Solution
#
# File: des_stat_measures_sample_sol.R
#
#####################################################

library(tidyverse)

# Consider the observations 
x <- c(4,3,2,4,10)

# a) mean
mean(x)

# b) median
x.ordered <- sort(x)  # ordered sample
x.ordered[ceiling((length(x))/2)]

# mention that median() function does not always get the same result; for 
# example median(c(x,1)) is 3.5 but according to our definition it is 3
# quantile() with type=1 is equivalent to our definition
quantile(x, p=0.5, type=1)

# c) mode
x %>% tibble(val = x) %>%
  # find the absolute frequencies of the values
  group_by(val) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  # find the entries with max. abs. frequency
  filter(n == max(n)) %>%
  # select only value
  select(val) %>%
  # remove duplicate entries
  unique()

# 20% quantile
x.ordered[ceiling(0.2* length(x))]
# alternative
quantile(x, p=0.2, type=1)

# trimmed 40% mean
# mean(x, trim = 0, na.rm = FALSE, ...)
# x	R object
# trim the fraction (0 to 0.5) of observations to be trimmed from each end of x 
# before the mean is computed. Values of trim outside that range are taken as 
# the nearest endpoint.
mean(x,trim = 0.2)
# alternative
x.ordered[floor(0.2*length(x)+1):ceiling(0.8*length(x))] %>%
  mean()