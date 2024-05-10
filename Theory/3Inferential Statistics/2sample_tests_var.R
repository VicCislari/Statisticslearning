##########################################################################
#
# Examples for two sample variance tests in the normal model
#
# file: 2sample_tests_var.R
##########################################################################

##########################################################################
# F-Test: comparison of variances of independent, normally distributed
#         random variables 
##########################################################################
# Example: Lehn: S.145, 147
# example slide 21
x <- c(7.0,11.8,10.1,8.5,10.7,13.2,9.4,7.9,11.1)
y <- c(13.4,14.6,10.4,11.9,12.7,16.1,10.7,8.3,13.2,10.3,11.3,12.9,9.7)
mean(x); var(x) # 9.966667; 3.9
mean(y); var(y) # 11.96154; 4.572564
alpha <- 0.1
var.test(x,y,alternative="two.sided",conf.level=1-alpha)
#	F test to compare two variances
#
#data:  x and y
#F = 0.8529, num df = 8, denom df = 12, p-value = 0.8451
#alternative hypothesis: true ratio of variances is not equal to 1
#90 percent confidence interval:
# 0.2994185 2.8009147
#sample estimates:
#ratio of variances
#         0.8529131
qf(c(0.025,0.05,0.95,0.95,0.975), df1=8, df2=12)

# unpaired t-test: comparing the means
alpha <- 0.05
t.test(x,y,alternative="two.sided",mu=0,paired=FALSE,var.equal=TRUE,
       conf.level=1-alpha)
#	Two Sample t-test
#
#data:  x and y
#t = -2.2176, df = 20, p-value = 0.03833
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -3.8713259 -0.1184177
#sample estimates:
#mean of x mean of y
# 9.966667 11.961538