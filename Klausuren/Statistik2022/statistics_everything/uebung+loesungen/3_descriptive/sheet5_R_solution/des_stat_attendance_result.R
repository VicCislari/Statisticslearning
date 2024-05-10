#####################################################
# Descriptive Statistics: 
# association attendance and course results
#
# File: des_stat_attendance_result.R
#
###################################################### 

# load packages
library(tidyverse)

# 3x2-contingency table: attendance, result
tab <- as.tibble(matrix(c(40,10,20,10,10,10),nrow=3,ncol=2,byrow=TRUE))
tab
# A tibble: 3 x 2
#V1    V2
#<dbl> <dbl>
#  1    40    10
#  2    20    10
#  3    10    10

# indifference table
ind_tab <- as.tibble(
  matrix(rowSums(tab),nrow=3,ncol=1)   %*% 
    matrix(colSums(tab),nrow=1,ncol=2) / sum(tab)
)
ind_tab
# A tibble: 2 x 3
#V1    V2
#<dbl> <dbl>
# 35    15
# 21     9
# 14     6

# computation of Chi^2, C and C_corr
chi_2 <- sum((tab-ind_tab)^2/ind_tab)
chi_2 # 6.349206
C <- (chi_2/(chi_2+sum(tab)))^0.5
C # 0.2443389
C_korr <- ((min(2,3)/(min(2,3)-1)) *chi_2/(chi_2+sum(tab)))^0.5
C_korr # 0.3455474

############################################################
# solution applying R functions
############################################################
# generate raw data
raw.data <- tibble(
  attendance = c(rep("over 70%",50), rep("30% - 70%",30), rep("under 30%",20)),
  result = c(rep("pass",40), rep("fail",10),
             rep("pass",20), rep("fail",10),
             rep("pass",10), rep("fail",10)))
raw.data

# contingency table
table(raw.data)

# contingency and indifference tabel, Chi^2 from chisq.test
res <- chisq.test(x = raw.data$attendance, y=raw.data$result)
res
# contingency table
res$observed
# indifference table
res$expected
# Chi^2
res$statistic
