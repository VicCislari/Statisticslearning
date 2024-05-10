##########################################################################
# Exercise: chi^2 test:association of two qualitative numbers
# 
# file: infstat_2samples_testing_number_position.R
##########################################################################
library(tidyverse)

# Some parents of the West Bay little leaguers think that they
# are noticing a pattern. There seems to be a relationship between
# the number on the kids' jerseys and their position. These parents
# decide to record what they see. The hypothetical data appear
# below. Conduct a Chi Square test to determine if the parents'
# suspicion that there is a relationship between jersey number and
# position is right. 

conttab <- matrix(c(12,5,5,5,10,2,4,4,7), nrow = 3, ncol = 3, byrow = TRUE)

conttab %>% addmargins()

# chi^2 test: association of two qualitative variables
# H0: no association
res <- chisq.test(x=conttab)
res
# contingencytable
res$observed %>% addmargins()
# indifferenctable
res$expected %>% addmargins()
# Chi^2
res$statistic
# decision: reject H0, if chi^2 > (1-alpha) quantile of chi^2 
# distribution with k = (3-1)(3-1)=4
res$statistic > qchisq(0.95,df=4)
# reject H0, since p-value = 0.03679
1-pchisq(10.22573, df=4)
  
