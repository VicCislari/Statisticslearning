#####################################################
# Descriptive Statistics: measures of a frequency table
#
# File: des_stat_freq_tab_measures.R
#
#####################################################
library(tidyverse)

# frequency table
freq_tab <- tibble(
  no = 1:8,
  nobs = c(5,4,1,7,2,3,1,2)
)
freq_tab

# ordered raw data
x <- rep(freq_tab$no, freq_tab$nobs)

# mean
mean(x)
# geometric mean
prod(x)^(1/length(x))
# harmonic mean
length(x)/sum(1/x)
# trimmed 20% mean
mean(x, trim = 0.1)