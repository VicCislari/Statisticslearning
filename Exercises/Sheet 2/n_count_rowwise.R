############################################################
# Sheet 2: n(), count() and rowwise()
#
# file n_count_rowwise.R
############################################################

library(tidyverse)

no <- 30
exercise.results <- tibble(
  stud.id = 1:no,
  group = sample(x=c("A","B","C"), size=no, replace = TRUE),
  ex1 = sample(x=1:10, size=no, replace = TRUE),
  ex2= sample(x=1:10, size=no, replace = TRUE),
  ex3 = sample(x=1:10, size=no, replace = TRUE),
  ex4 = sample(x=1:10, size=no, replace = TRUE),
  ex5 = sample(x=1:10, size=no, replace = TRUE)
)
exercise.results

# a) Apply n() and count() to get the number of students in the different
#    groups. What are the difference between n() and count()?
exercise.results %>%
  group_by(group) %>%
  summarise(no.students = n())
# count() = first group_by() and and than n().
exercise.results %>%
  count(group)

# b) Add the variables sum.scores and mean.scores containing the sum 
#    and the of the scores in the exercises for every student by applying the
#    the functions sum() and mean(). What is the result if rowwise() is applied
#    before the mutate()?
exercise.results %>%
  # Without rowwise() the sum resp. the mean of all columns ex1, ..., ex5 are
  # calculated. Applying rowwise() theses results are rowwise evaluated.
  rowwise() %>%
  mutate(
    sum.scores = sum(c(ex1,ex2,ex3,ex4,ex5)),
    mean.scores = mean(c(ex1,ex2,ex3,ex4,ex5))
  )
