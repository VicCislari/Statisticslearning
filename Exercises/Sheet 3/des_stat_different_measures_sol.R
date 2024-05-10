#####################################################
# Descriptive Statistics: changing measures
# Solution
#
# File: des_stat_different_measures_sol.R
#
#####################################################
# Make up data sets with 5 numbers each that have:
# a) the same mean but different standard deviations.
xa <- c(1,3,5,7,9)
ya <- c(3,4,5,6,7)
mean(xa); mean(ya)
# 5        2
sd(xa); sd(ya)
# sd(xa)    sd(ya)
# 3.162278  1.581139

# b) the same mean but different medians.
xb <- c(1,3,5,7,9)
yb <- c(1,3,6,7,8)
mean(xb); mean(yb); median(xb); median(yb)
# 5         5          5           6

# c) the same median but different means.
xc <- c(1,3,5,7,9)
yc <- c(1,3,5,7,14)
mean(xc); mean(yc); median(xc); median(yc)
#  5        6           5          5
