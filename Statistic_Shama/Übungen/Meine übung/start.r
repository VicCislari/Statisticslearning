library(tidyverse)
student1 <- tibble  (
  student = c("Adam","Bernd","Christian","Doris"),
  algebra = c(NA, 5, 3, 4),
  analysis = c(2, NA, 1,3),
  diskrete.math = c(3,NA,2,4),
)

student1 %>% gather("algebra","analysis","diskrete.math", key = "exam", value = "grade") %>% unique


student1 %>% gather("algebra", "analysis", "diskrete.math", key = "exam", value = "noten")














