##########################################################################
# Exercise: chi^2 goodness of fit test: discrete distrobution
# 
# file: infstat_2samples_testing_girls_family.R
##########################################################################

# In 380 randomly selected families with four children each
# it is investigated how many of them are girls. The result is the
# following findings:
girls <- 0:4
fam <- c(25,95,150,80,30)
# Does this finding correspond to the hypothesis that the variable
# ``number of girls in families with four children each'' follows a
# Binomial distribution with n = 4 and p = 0.5?  Test this
# hypothesis at a significance level of 0.1!

# H0: fam ~ B(n=4,p=0.5)
alpha <- 0.1
chisq.test(fam, p = dbinom(0:4,size = 4, prob = 0.5))
# do not reject H0, since p-value = 0.3457
  
