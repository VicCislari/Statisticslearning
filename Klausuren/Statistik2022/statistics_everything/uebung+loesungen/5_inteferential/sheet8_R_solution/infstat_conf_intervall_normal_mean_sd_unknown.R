##################################################
# You take a sample of 22 from a population of test 
# scores, and the mean of your sample is 60.
#
# file: infstat_conf_intervall_normal_mean_sd_unknown.R
###################################################
n <- 22
m <- 60

# a) You know the standard deviation of the population is 10. What
#    is the 99\% confidence interval on the population mean.
alpha <- 0.01
s <- 10
q_a <- qnorm(1-alpha/2,0,1)
q_a
u <- m-q_a*s/sqrt(n)
o <- m+q_a*s/sqrt(n)
u;o


# Solution applying z.test() from the TeachingDemos package
library(TeachingDemos)
z.test(x = m, sd = 10, alternative = "two.sided", n = 22, conf.level = 0.99)$conf.int 

# b) Now assume that you do not know the population standard
#    deviation, but the standard deviation in your sample is 10. What
#    is the 99\% confidence interval on the mean now?
s_sample <- 10
t_a <- qt(1-alpha/2,n-1)
t_a
u <- m-t_a*s/sqrt(n)
o <- m+t_a*s/sqrt(n)
u;o