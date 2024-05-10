library (tidyverse)
student1 <- tibble(
  student = c("Adam","Bernd","Christian","Doris"),
  algebra = c(NA, 5, 3, 4),
  analysis = c(2, NA, 1,3),
  diskrete.math = c(3,NA,2,4),
)
student1

####################################################################
 student2 <- tibble(
#noten und klausuren in einer liste

 student1 %>%
  gather('algebra','analysis', 'diskrete.math', key = "exam", value = "grade"),


   name = rep(c("Adam", "Bernd", "Christian", "Doris"), each = 2),
   type = rep(c("height", "weight"), 4),
   measure = c(1.83, 81, 1.75, 71, 1.69, 55, 1.57, 62))

 # in eine reihe bringen den hight und weight

student2 %>%
 spread( key = type ,value = measure)
###########################################

student3 <- tibble(
  name = c("Adam", "Bernd", "Christian", "Doris"),
  ratio = c("81/1.83", "71/1.75", "55/1.69", "62/1.57"))



#in eine reihe gewicht und gröse

student3 %>%

  separate(col = ratio, into= c("weight", "height"),sep="/")















