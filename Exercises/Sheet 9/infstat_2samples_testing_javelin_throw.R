##########################################################################
# Exercise: 2 sample t-test, Welsh-test
# 
# file: infstat_2samples_testing_javelin_throw.R
##########################################################################

# 2 independent random variable with unknown but equal resp. not equal variances

# Aufgabensammlung Lehn: Nr. 128
# To test two training methods A and B for javelin throw, 60
# untrained physical education students were randomly divided into
# two groups of m=25 and n=35 students, respectively. Before the
# start of the training phase First, a performance test was conducted
# and for each student the distance of the best of two throws was
# noted. After completion of the training phase, during which the
# students in group 1 were method A and the students of group 2 were
# trained according to method B, was trained, a corresponding
# performance test was performed. The following results were obtained
# for the differences between the values obtained in the second and
# the first performance test:
x <- c(7.06,11.84,9.28,7.92,13.5,3.98,3.82,7.34,8.7,9.24,4.86,3.32,
       12.78,12,5.24,11.4,6.56,9.04,7.72,9.26,7.88,8.6,9.3,8.42,8.54)
y <- c(8.68,6,6.3,10.24,10.88,5.36,7.82,4.7,9.02,9.78,6.9,
       5.8,13.56,10.32,13.3,11.38,7.94,10.74,13.68,14.92,7.42,10.36,
       10.54,5.22,13.74,12.98,10.34,10.02,17.8,13.04,5.2,9.4,11.18,
       12.68,12.36)
# Which hypothesis do you have to test if you want to show that the
# Method B is better than Method A? Perform an corresponding test at
# the level alpha = 0.05. 


# H0: mu.x >= mu.y, H1: mu.x < mu.y

##########################################################################
# case: equal variances
alpha <- 0.05
t.test(x,y,alternative="less",mu=0, paired=FALSE,var.equal=TRUE,
       conf.level=1-alpha)
# reject H0, since p-value = 0.0181

##########################################################################
# case: not equal variances
alpha <- 0.05
t.test(x,y,alternative="less",mu=0,paired=FALSE,var.equal=FALSE,
       conf.level=1-alpha)
# reject H0, since p-value = 0.01596
