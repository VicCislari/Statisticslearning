##########################################################################
# Exercise: 2 sample Gauss test
#
# file: infstat_2samples_testing_screwnuts.R
##########################################################################

# Aufgabensammlung Lehn 121
# Two machines produce screw-nuts. The diameters of the screw-nuts
# from machine 1 resp. 1  are normally distributed with standard deviation
# sigma_1 = 0.5 resp. sigma_2=0.6 and unknown means.

sx <- 0.5; sy <- 0.6
x <- c(5.46,5.34,4.34,4.82,4.40,5.12,5.69,5.53,4.77,5.82)
y <- c(5.45,5.31,4.11,4.69,4.18,5.05,5.72,5.54,4.62,5.89,5.60,
       5.19,3.31,4.43,5.30,4.09)
nx <- length(x); ny <- length(y)
test.stat <- (mean(x)-mean(y))/sqrt(sx^2/nx + sy^2/ny)
alpha <- 0.05

# one sided: reject H0 mx >= my,if test.stat < quantile
quantile <- qnorm(alpha,0,1)
pvalue <- pnorm(test.stat,0,1)
test.stat;quantile;pvalue # 1.027782; -1.644854; 0.8479739