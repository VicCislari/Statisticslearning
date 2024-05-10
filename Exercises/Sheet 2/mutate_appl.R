###############################################
# Sheet II: creating and manipulating variables
#
# file: mutate_appl.R
###############################################

# load libraries
library(tidyverse)

df <- tibble(
  id = 1:10,
  sex = sample(x =c("f","m"), size = 10,
               replace = TRUE),
  age = round(runif(10,20,35)),
  score1 = round(runif(10,0,25))
)
df

# a)
df %>% filter(sex == "m")

# b)
df <- df %>% 
  add_row(id = 11, sex = "m", age = 25, score1 = 4)
df

# c)
df <-
  df %>%
  mutate(score2 = round(runif(11,0,25)),
         score3 = round(runif(11,0,25)),
         scoresum = score1+score2+score3, 
         grade = case_when(
           scoresum <= 37 ~ 5,
           scoresum > 37 & scoresum <= 45 ~ 4,
           scoresum > 45 & scoresum <= 55 ~ 3,
           scoresum > 55 & scoresum <= 65 ~ 2,
           scoresum > 65 ~ 1))
df

# d)
df %>% 
  select(id,sex,grade) %>%
  filter(grade < 5) %>%
  arrange(sex) 

# e)
df %>%
  group_by(sex) %>%
  summarise(mean_scores = mean(scoresum),
            min_scores = min(scoresum),
            max_scores = max(scoresum),
            med_scores = median(scoresum))

