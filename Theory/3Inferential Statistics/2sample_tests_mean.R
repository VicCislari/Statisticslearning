##########################################################################
# Examples of two sample tests for the expected value
#
# file: 2sample_tests_mean.R
##########################################################################


##########################################################################
# Normal Model
# a) Gauﬂ-Test: 2 independent random variables with known variances
# b) t-Test: 2 independent random variables with unknown but equal variances
# c) Welsh Test: 2 independent random variables with unknown but different 
#    variances
# d) 2-paired t-test: 2 dependent random variables with unknown but equal 
#    variances
##########################################################################


# a) Gauﬂ-Test: 2 independent random variables with known variances
# Sample: Aufgabensammlung Lehn: Nr. 121
# example slide 8
sx <- 0.5; sy <- 0.6
x <- c(5.46,5.34,4.34,4.82,4.40,5.12,5.69,5.53,4.77,5.82)
y <- c(5.45,5.31,4.11,4.69,4.18,5.05,5.72,5.54,4.62,5.89,5.60,
       5.19,3.31,4.43,5.30,4.09)
nx <- length(x); ny <- length(y)
test.stat <- (mean(x)-mean(y))/sqrt(sx^2/nx + sy^2/ny)
alpha <- 0.05

# H0: mx <= my
# one sided: reject H0, if test.stat > 1-alpha quantil
quantil <- qnorm(1-alpha,0,1)
pvalue <- 1-pnorm(test.stat,0,1)
test.stat;quantil;pvalue 

# b) t-Test: 2 independent random variables with unknown but equal variances

# Example: Aufgabensammlung Lehn: Nr. 125, example slide 11
nx <- 15; ny <- 20; mx <- 72; sx <- 13; my <- 75; sy <- 12
test.stat <- sqrt(nx*ny*(nx+ny+2)/(nx+ny)) *
  (mx - my)/sqrt((nx-1)*sx^2 +(ny-1)*sy^2)

# reject H0: m1=m2 ab, if |test.stat| > quantile
alpha <- 0.05
quantile <- qt(1-alpha/2,nx+ny-2)
pvalue <- 2*(1-pt(abs(test.stat),df=nx+ny-2))
test.stat; quantile; pvalue 

# Example: Aufgabensammlung Lehn: Nr. 128
x <- c(7.06,11.84,9.28,7.92,13.5,3.98,3.82,7.34,8.7,9.24,4.86,3.32,
       12.78,12,5.24,11.4,6.56,9.04,7.72,9.26,7.88,8.6,9.3,8.42,8.54)
y <- c(8.68,6,6.3,10.24,10.88,5.36,7.82,4.7,9.02,9.78,6.9,
       5.8,13.56,10.32,13.3,11.38,7.94,10.74,13.68,14.92,7.42,10.36,
       10.54,5.22,13.74,12.98,10.34,10.02,17.8,13.04,5.2,9.4,11.18,
       12.68,12.36)
alpha <- 0.05
t.test(x,y,alternative="less",mu=0,paired=FALSE,var.equal=TRUE,
       conf.level=1-alpha)

#	Two Sample t-test
#data:  x and y
#t = -2.1444, df = 58, p-value = 0.01810
#alternative hypothesis: true difference in means is less than 0
#95 percent confidence interval:
#       -Inf -0.3714405
#sample estimates:
#mean of x mean of y
# 8.304000  9.988571


# c) Welsh test, i.e. we assume that the variances are not equal
# and unknown.
# example from two sample Gauﬂ test: slide 13
x <- c(5.46,5.34,4.34,4.82,4.40,5.12,5.69,5.53,4.77,5.82)
y <- c(5.45,5.31,4.11,4.69,4.18,5.05,5.72,5.54,4.62,5.89,5.60,
       5.19,3.31,4.43,5.30,4.09)
nu <- ( sd(x)^2/length(x) +sd(y)^2/length(y) )^2 / 
         ( (sd(x)^2/length(x))^2/(length(x)-1) + (sd(y)^2/length(y))^2/(length(y)-1) )
nu

t.test(x,y, alternative = "less", paired = FALSE, var.equal = FALSE,
       conf.level = 1-alpha)
# Welch Two Sample t-test
# data:  x and y
# t = 0.9086, df = 23.372, p-value = 0.1864
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   -0.1982461        Inf
# sample estimates:
#   mean of x mean of y 
#   5.129     4.905 


# d) 2-paired t-test: 2 dependent random variables with unknown but equal 
#    variances

# Example Online Statistics: Chapter 10 Section Correlated Pairs
# slides 16, 17
D0 <- c(57,27,32,31,34,38,71,33,34,53,
        36,42,26,52,36,55,36,42,36,
        54,34,29,33,33)
D60 <- c(62,49,30,34,38,36,77,51,45,42,
         43,57,36,58,35,60,33,49,33,
         59,35,37,45,29)

mean(D60-D0) # 4.96

alpha <- 0.05
qt(1-alpha/2, df = length(D0))

t.test(D0,D60,alternative="two.sided",mu=0,paired=TRUE,var.equal=TRUE,
       conf.level=1-alpha)
#	Paired t-test
#
#data:  D0 and D60
#t = -3.2224, df = 23, p-value = 0.003771
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -8.141431 -1.775236
#sample estimates:
#mean of the differences
#              -4.958333

# Ergebnis bei falscher Annahme 2er unabh‰ngiger Stichproben
t.test(D0,D60,alternative="two.sided",mu=0,paired=FALSE,var.equal=TRUE,
       conf.level=1-alpha)
