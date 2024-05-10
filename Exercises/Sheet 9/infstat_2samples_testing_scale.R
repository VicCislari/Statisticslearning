##########################################################################
# Exercise: F-test
# 
# file: infstat_2samples_testing_scale.R
##########################################################################
# Aufgabensammlung Lehn, Wegmann 132

# A company manager is considering whether to purchase a new type B
# scale on the market. A new acquisition should be made only if the
# type B scale is better than the type A scale used up to now. For
# the evaluation of the quality of a scale, the scatter of weighing
# results should be used. be used. In weight measurements for one
# and the same weight the following results were obtained the
# following measured values for the individual scales: 
x <- c(102.4,101.3,97.6,98.2,102.3,99.1,97.8,103.9,101.6,100.1)
y <- c(98.4,101.7,100.5,99.3,100.6,99.6,102.2,101.1,99.9,101.0)
# Under suitable normal distribution assumptions, check with a test
# with alpha=0.05 to determine whether a new acquisition makes
# sense.

# F-test: H0: sigma.x <= sigma.y
var.test(x, y, alternative = "greater", conf.level=0.95)
# reject H0, since p-value = 0.03404