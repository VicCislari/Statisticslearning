library(tidyverse)

student1 <- tibble  (
  student = c("Adam","Bernd","Christian","Doris"),
  algebra = c(NA, 5, 3, 4),
  analysis = c(2, NA, 1,3),
  diskrete.math = c(3,NA,2,4),
)
student1 %>%  gather("algebra", "analysis", "diskrete.math", key = "exam", value = "marks")

student2 <- tibble(
  name = rep(c("Adam", "Bernd", "Christian", "Doris"), each = 2),
  type = rep(c("height", "weight"), 4),
  measure = c(1.83, 81, 1.75, 71, 1.69, 55, 1.57, 62))

student2 %>% spread(key = type, value = measure)

student3 <- tibble(
  name = c("Adam", "Bernd", "Christian", "Doris"),
  ratio = c("81/1.83", "71/1.75", "55/1.69", "62/1.57"))

student3 %>% separate(col = "ratio", into = c("height", "weight"), sep = "/") 

df <- tibble(
            id = 1:10,
            gender = sample (c ("f", "m"), size = 10, replace =(TRUE)),
            age= round(runif (10,20,35)),
            score1 = round (runif(10,0,25))
            
  )
df
df %>%  filter(gender == "m")

df %>%  add_row( id = 11, gender = "m", age = 25, score1 = 4 )

f <- df %>% mutate(score2 = round(runif(10,0,25))) %>% 
            mutate(score3 = round(runif(10,0,25))) %>% 
            mutate(sumscore = (score1 + score2 + score3) )

f


















s <- df %>%          mutate(score2 = round(runif (10,0,25))) %>% 
                mutate(score3 = round(runif (10,0,25))) %>%
                mutate(scoresum =( score1+score2 + score3)) %>% 
                mutate(grade = case_when(
                  scoresum <= 37 ~ 5,
                  scoresum > 37 & scoresum <= 45 ~ 4,
                  scoresum > 45 & scoresum <= 55 ~ 3,
                  scoresum > 55 & scoresum <= 65 ~ 2,
                  scoresum > 65 ~ 1
                  
                ))
s



