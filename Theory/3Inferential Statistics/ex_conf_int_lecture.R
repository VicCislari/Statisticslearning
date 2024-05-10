####################################################
# Examples: Confidence Intervals 
#
# file: ex_conf_int_lecture.R
####################################################

# Normal Model

# a) confidence interval of the expected value: sigma known
sigma <- 10
alpha <- 0.05
n <- 25
X.bar <- 177.52
l <- X.bar-sigma*qnorm(1-alpha/2,mean = 0, sd = 1)/sqrt(n)
u <- X.bar+sigma*qnorm(1-alpha/2,mean = 0, sd = 1)/sqrt(n)
l; u

# b) confidence interval of the expected value: sigma unknown
S.n <- 8.227
alpha <- 0.05
n <- 25
X.bar <- 177.52
l <- X.bar-S.n*qt(1-alpha/2,df = n-1)/sqrt(n)
u <- X.bar+S.n*qt(1-alpha/2,df = n-1)/sqrt(n)
l; u

# c) confidence interval of sigma^2: mu known
Q.n <- 1778
alpha <- 0.05
n <- 25
l <- Q.n/qchisq(1-alpha/2, df = n)
u <- Q.n/qchisq(alpha/2, df = n)
l; u

# c) confidence interval of sigma^2: mu unknown
S2.n <- 67.68
alpha <- 0.05
n <- 25
l <- (n-1)*S2.n/qchisq(1-alpha/2, df = n-1)
u <- (n-1)*S2.n/qchisq(alpha/2, df = n-1)
l; u

# Binomial Model
X <- 12
n <- 40
alpha <- 0.05
c <- qnorm(1-alpha/2, mean = 0, sd = 1)
# normal approximation
l.normal.approx <- (X+c^2/2-c*(X*(n-X)/n+c^2/4)^0.5)/(c^2+n)
u.normal.approx <- (X+c^2/2+c*(X*(n-X)/n+c^2/4)^0.5)/(c^2+n)
l.normal.approx; u.normal.approx
# normal approximation: big n
p.hat <- X/n
l.normal.approx.big <- p.hat-qnorm(1-alpha/2, mean=0, sd=1)*(p.hat*(1-p.hat)/n)^0.5
u.normal.approx.big <- p.hat+qnorm(1-alpha/2, mean=0, sd=1)*(p.hat*(1-p.hat)/n)^0.5
l.normal.approx.big; u.normal.approx.big
# exact values
binom.test(x = X, n = n, alternative = "two.sided", conf.level = 1-alpha)$conf.int