##########################################################################
# Exercise: chi^2 homogeneity and independence test
# 
# file: infstat_2samples_testing_titanic.R
##########################################################################
library(tidyverse)

# The passengers rescued from the titanic depending on the travel class is
# given in the following table
cont.tab <- matrix(c(202,125,180,211,135,160,541,674),
                   nrow = 2, ncol = 4, byrow = TRUE)
cont.tab %>% addmargins

# Check with an approbriate test whether the ``rescue status'' and the
# ``travel class'' are independent and whether the conditional
# probabilities of ``rescue status'' given ``travel class'' differ
# by ``travel class''.

# chi^2 indenpendence and homogeneity test
res <- chisq.test(cont.tab)
# contingency table
res$observed
# indifference table
res$expected
# chi^2
res$statistic
# p-value
res$p.value

# The chi^2 independence test and chi^2 homogeneity are technically identical.
# The null hypothesis are quite different:
# chi^2 independence test: H0 = "rescue status" and "travel class" are 
#                                independent
# chi^2 homogeneity test: H0: the proportion of passengers rescued is identical
#                             for the different travel classes
# conclusion: since the p-value is rather low both null hypothesis can be
# rejected


