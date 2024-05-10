#####################################################
# Descriptive Statistics: Returns of a portfolio
# Solution
#
# File: des_stat_returns_sol.R
#
#####################################################
# Consider a stock portfolio that began with a value 
# of 1000 and had annual returns of 13%, 22%, 12%, -5%, 
# and -13%.
# a) Compute the value after each of the five years.
x <- 1000
ret <- c(0.13,0.22,0.12,-0.05,-0.13)
value <- 1000 * cumprod(1+ret)
value
#  1130.000 1378.600 1544.032 1466.830 1276.142

# Compute the annual rate of return.
annual_rate <-  (prod(1+ret)^0.2 -1)*100
annual_rate

# wrong annual rate
mean(ret)
# value after 5 years using wrong.rate
1000*(1+mean(ret))**5


#  4.997711
# expected return after year 6
value[5]* (1+annual_rate/100)
# expected return after year 7
value[5]* (1+annual_rate/100)**2
