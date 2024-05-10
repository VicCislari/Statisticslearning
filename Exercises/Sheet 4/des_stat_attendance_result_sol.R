#####################################################
# Descriptive Statistics: 
# association attendance and course results
# Solution
#
# File: des_stat_attendance_result_sol.R
#
###################################################### 
library(tidyverse)

# 3x2-contingency table: attendance, result
tab <- matrix(c(40,10,20,10,10,10),nrow=3,ncol=2,byrow=TRUE)
tab

# indifference table
# indifference table
ind_tab <- 
  matrix(rowSums(tab),nrow=3,ncol=1)   %*% 
  matrix(colSums(tab),nrow=1,ncol=2) / sum(tab)
# or
chisq.test(tab)$expected

# computation of Chi^2, C and C_corr
chisq.test(tab)$statistic
# or
chi_2 <- sum((tab-ind_tab)^2/ind_tab)
chi_2 # 6.349206
C <- (chi_2/(chi_2+sum(tab)))^0.5
C # 0.2443389
C_korr <- ((min(2,3)/(min(2,3)-1)) *chi_2/(chi_2+sum(tab)))^0.5
C_korr # 0.3455474
