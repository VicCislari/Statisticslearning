####################################################
# Calculate for the given sample from normally 
# distributed population the 95% confidence intervals
# a) for the mean, if the standard deviation is 2
# b) for the mean, if the standard deviation is unknown
# a) for the variance, if the mean is 250
# a) for the variance, if the mean is unknown
# 
# file: infstat_conf_intervall_normal_mu_sigma.R
#####################################################

# create sample values
# s.values <- round(rnorm(n=20, mean = 251, sd = 2),1)
s.values <- c(247.4,249.0,248.5,247.5,250.6,252.2,253.4,248.3,251.4,246.9,
              249.8,250.6,252.7,250.6,250.6,252.5,249.4,250.6,247.0,249.4)
# characteristics of the sample
n <- length(s.values)
xbar <- mean(s.values)
s <- sd(s.values)
# level 1-alpha
alpha <- 0.05

# confidence intervalls for mu
# a) assumption: sigma = 2
sigma <- 2
l.a <- xbar - qnorm(1-alpha/2)*sigma/sqrt(n)
u.a <- xbar + qnorm(1-alpha/2)*sigma/sqrt(n)
l.a; u.a
# b) assumption: sigma = unknown
l.b <- xbar - qt(1-alpha/2, df = n-1)*s/sqrt(n)
u.b <- xbar + qt(1-alpha/2, df = n-1)*s/sqrt(n)
l.b; u.b

# confidence intervalls for sigma^2
# c) assumption: mu = 250
mu <- 250
Qn <- sum((s.values - mu)^2)
l.c <- Qn/qchisq(1-alpha/2, df = n)
u.c <- Qn/qchisq(alpha/2, df = n)
l.c; u.c
# d) assumption: mu unknown
l.d <- (n-1)*s^2/qchisq(1-alpha/2, df = n-1)
u.d <- (n-1)*s^2/qchisq(alpha/2, df = n-1)
l.d; u.d

# solutions applying z.test(), sigma.test() from TeachingDemos and t.test()
library(TeachingDemos)
z.test(x = s.values, sd = 2, alternative = "two.sided", conf.level = 0.95)$conf.int # a)
t.test(x = s.values, alternative = "two.sided", conf.level = 0.95)$conf.int         # b)
sigma.test(x = s.values, alternative = "two.sided", conf.level = 0.95)              # d)
