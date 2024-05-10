#######################################################################
# Example: chi^2-test - association of qualitative variables
#
# file: chi2_tests_qual.r
#######################################################################

library(tidyverse)

# Example: Data from a Mediterranean Diet and Health case study
#                          & Fatal Heart & Non-Fatal Heart 
#  Diet          & Cancers & Disease     & Disease         & Healthy & Total
#  AHA           & 15      & 24          & 25              & 239     & 303
#  Mediterranean & 7       & 14          & 8               & 273     & 302
#  Total         & 22      & 38          & 33              & 512     & 605 

# generate a the contingency table
cont.tab <- matrix(c(15,24,25,239,7,14,8,273), nrow = 2, ncol = 4, byrow = TRUE)

# chi^2-test
res <- chisq.test(x = cont.tab)

# contingencytable
res$observed %>% addmargins()
# indifferenectable
res$expected %>% addmargins()
# Chi^2
res$statistic

qchisq(0.99, df=3)
