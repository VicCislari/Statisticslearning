##########################################################################
# Exercise: 2 unpaired sample t-test and F-test
# 
# file: infstat_2samples_testing_cattle_feed.R
##########################################################################

# Aufgabensammlung Lehn, Wegmann Aufgabe 130
# On one farm, 10 cattle were fed (group 1) were fed concentrates of
# composition A, and the remaining 10 cattle (group 2) were fed the
# conventional conventional feed of composition B. After a certain
# time weight gain was noted in both groups: 

x <- c(7.2,4.1,5.5,4.5,5.7,3.8,4.6,6.0,5.2,5.4)
y <- c(5.3,4.4,5.0,3.5,3.9,4.9,5.6,2.5,4.0,3.6)

# Assuming that weight gain can be described by independent
# random variables that are identically normally distributed in
# both cases, use an appropriate test at level alpha=0.1 to
# test whether to reject the assumption that weight gain from
# administration of composition A concentrate has the same
# dispersion as weight gain from administration of the
# conventional composition B diet. 

mean(x); var(x) 
mean(y); var(y) 
alpha <- 0.1
var.test(x,y,alternative="two.sided",conf.level=1-alpha)
# do not reject H0, that the variances are equal

# Assuming that weight gain can be described by independent
# random variables with equal variance, identically normally
# distributed in each of the two cases, use an appropriate test at
# the level alpha=0.05 to test whether the hypothesis that
# weight gain with administration of concentrate of composition A
# is not greater than weight gain with administration of the
# conventional diet of composition B is correct. 

alpha <- 0.025
t.test(x,y,alternative="greater",paired=FALSE,var.equal=TRUE,
       conf.level=1-alpha)
# reject H0, that the mu.x < mu.y since p-value = 0.0181
