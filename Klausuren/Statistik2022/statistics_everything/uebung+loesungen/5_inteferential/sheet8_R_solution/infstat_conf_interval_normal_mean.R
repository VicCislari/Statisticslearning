##################################################
# A population is known to be normally distributed 
# with a standard deviation of 2.8.
#
# file: infstat_conf_interval_normal_mean.R
##################################################

# a) Compute the 95% confidence interval on the mean
sample <- c(8, 9, 10, 13, 14, 16, 17, 20, 21)
alpha <- 0.05
m <- mean(sample)
m
s <- 2.8
q_a <- qnorm(1-alpha/2,0,1)
q_a
u <- m-q_a*s/sqrt(length(sample))
o <- m+q_a*s/sqrt(length(sample))
u;o

# b) Now compute the 99% confidence interval using the same data.
alpha <- 0.01
q_a <- qnorm(1-alpha/2,0,1)
q_a
u <- m-q_a*s/sqrt(length(sample))
o <- m+q_a*s/sqrt(length(sample))
u;o

# Solution applying z.test() from the TeachingDemos package
library(TeachingDemos)
z.test(x= sample, sd = 2.8, alternative = "two.sided", conf.level = 0.95)$conf.int  # a)
z.test(x = sample, sd = 2.8, alternative = "two.sided", conf.level = 0.99)$conf.int # b)