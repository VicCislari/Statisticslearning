###############################################
# Sheet II: tidy and messy data
#
file: tidy_messy.R
###############################################

# load libraries
library(tidyverse)

# Create the datasets
student1  <- tibble(
  student = c("Adam","Bernd","Christian","Doris"),
  algebra = c(NA, 5, 3, 4),
  analysis = c(2, NA, 1,3),
  diskrete.math = c(3,NA,2,4)
)
student1

student2 <- tibble(
  name = rep(c("Adam", "Bernd", "Christian", "Doris"), each = 2),
  type = rep(c("height", "weight"), 4),
  measure = c(1.83, 81, 1.75, 71, 1.69, 55, 1.57, 62))
student2

student3 <- tibble(
  name = c("Adam", "Bernd", "Christian", "Doris"),
  ratio = c("81/1.83", "71/1.75", "55/1.69", "62/1.57"))
student3

# student1: contains 36 values representing three variables and 12 observations. 
# The variables are: name, exam, grade
# Every combination of name and exam is a single measured observation. The 12 
# single measured observations are the values of grade for every name and exam.

# student2 and student3: contains 12 values representing three variables and 
# 3 measurements for every student. The variables are: name, height, weight
# The 4 single measured observations are the values of height and weight for every name.

# Why are these datasets are not tidy?

# student1: column headers are values of the variable exam, not variable names
# student2: the variables weight and height are stored in both rows and columns
# student3: the values of the variables height and weight are stored in one column

# tidy versions
student1 %>% 
  gather('algebra','analysis','diskrete.math', 
         key = "exam", value = "grade")

student2 %>%
  spread(key = type, value = measure)

student3 %>% 
  separate(col = ratio, into = c("weight","height"), sep = "/") %>%
  mutate(ratio = as.double(height)/as.double(weight)) 