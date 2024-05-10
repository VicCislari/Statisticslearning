###############################################
# Sheet II: Exercises Datamanipulations with R
###############################################

# load libraries
library(tidyverse)

###############################################
# Task 1 Tidy Data

# Create the datasets
student1  <- tibble(
  student = c("Adam","Bernd","Christian","Doris"),
  algebra = c(NA, 5, 3, 4),
  analysis = c(2, NA, 1,3),
  diskrete.math = c(3,NA,2,4),
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
# Every combination of name and exam is a single measured observation. 

# student2 and student3: contains 12 values representing three variables and 3 observations. 
# The variables are: name, height, weight
# The 3 single measured observations are the values of height and weight for every name.

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
  separate(col = ratio, into = c("weight","height"), sep = "/")

###############################################
# Task 2 %>%-Operator
# a)
sin(log((5+3)**0.5))
# or 
(5+3) %>% sqrt() %>% log() %>% sin()

# b)
v <- seq(from = 0.5, to = 5, by = 0.5)
v

round(sum(log(v**0.5)),2)
# %>%-operator
v %>% sqrt() %>% log() %>% sum() %>% round(2) 

###############################################
# Task 3
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
df <- add_row(df, id = 11, sex = "m", age = 25, score1 = 4)
df

# c)
df <-
  df %>%
  mutate(score2 = round(runif(11,0,25))) %>%
  mutate(score3 = round(runif(11,0,25))) %>%
  mutate(scoresum = score1+score2+score3) %>%
  mutate(grade = case_when(
    scoresum <= 37 ~ 5,
    scoresum > 37 & scoresum <= 45 ~ 4,
    scoresum > 45 & scoresum <= 55 ~ 3,
    scoresum > 55 & scoresum <= 65 ~ 2,
    scoresum > 65 ~ 1))
df

# d)
df %>% 
  arrange(sex) %>%
  select(id,sex,grade) %>%
  filter(grade < 5)

# e)
df %>%
  group_by(sex) %>%
  summarise(mean_scores = mean(scoresum),
            min_scores = min(scoresum),
            max_scores = max(scoresum),
            med_scores = median(scoresum))

###############################################
# Task 4
# a)
library(nycflights13)
?flights()

# b)
res.b <- 
  flights %>% filter(arr_delay > 120)
res.b

# c)
res.c <- 
  flights %>% filter(arr_delay > 120 & dep_delay <= 0)
res.c

# d)
res.d <- 
  flights %>% 
  filter(carrier %in% c("AA","DL","UA") & arr_delay <= 0)
res.d

# e)
res.e <- 
  flights %>%
  filter(carrier %in% c("UA","AA","DL") & month == 5 & arr_delay > 300) %>%
  select(carrier, flight) %>% 
  arrange(carrier,flight) %>%
  # remove multiple entries
  unique()
res.e

# f) Format HHMM or HMM, i.e. the last 2 numbers denote
# minute and the first or the first two are numbers denote
# the hour
# x %% y 	 modulus (x mod y) 5%%2 is 1
# x %/% y  integer division 5%/%2 is 2 
flights.new <-
  flights %>%
  mutate(dep_time = (dep_time %/% 100)*60 + dep_time %% 100, 
         arr_time = (arr_time %/% 100)*60 + arr_time %% 100) 
flights.new

# g)
res.g <- flights %>%
  mutate(speed = distance / air_time * 60) %>%
  #flights.new %>%
  select(carrier,flight,speed) %>%
  arrange(desc(speed)) %>% 
  top_n(10,speed)
res.g

# h)
res.h <- 
  flights %>% 
  # remove the NA's
  filter(!is.na(arr_delay)) %>%
  # boolean variable indicating a delay
  mutate(bool_del = if_else(arr_delay < 10,1,0)) %>%
  group_by(carrier) %>%
  # new columns: nof = number of flights,
  # ndel = number of delays, del_ratio = ratio 
  # values calculate per carrier
  mutate(nof = n(), 
         ndel = sum(bool_del), 
         del_ratio = ndel / nof) %>%
  select(carrier, nof, del_ratio) %>%
  # remove multiple entries
  unique() %>%
  arrange(desc(del_ratio))
res.h

# i)
res.i <-
  flights %>% 
  # remove NA's
  filter(!is.na(arr_delay)) %>%
  # boolean variable indicating a delay
  mutate(bool_del = if_else(arr_delay < 10,1,0)) %>%
  # Calculation grouped by carrier and month
  group_by(month,carrier) %>%
  # new columns: nof = number of flights,
  # ndel = number of delays, del_ratio = ratio 
  # values calculate per carrier
  mutate(nof = n(), 
         ndel = sum(bool_del), 
         del_ratio = ndel / nof) %>%
  # keep only 4 columns
  select(month, carrier, nof, del_ratio) %>%
  # calculation per month
  group_by(month) %>%
  # only highest ratio
  filter(del_ratio == max(del_ratio)) %>% 
  # remove multiple entries
  unique() %>%
  arrange(month)
res.i

# j) 3 tables are generated with values per month and day 
# which are joined by these variables
res.j <-
  inner_join(
    flights %>% 
      filter(is.na(dep_delay)) %>% 
      group_by(month, day) %>%
      # number of cancelled flights
      summarise(nof_canc = n())
    ,
    flights %>%
      group_by(month,day) %>%
      # number of no departure delays
      filter(dep_delay <= 5 & dep_delay >= -5) %>%
      summarise(nof_no_delay = n())
    ,
    by = c("month","day")
    ) %>%
  inner_join(
    flights %>%
      group_by(month,day) %>%
      # means
      summarise(mean_dep_del = mean(dep_delay, na.rm = TRUE), 
                mean_arr_del = mean(arr_delay, na.rm = TRUE))
    ,
    by = c("month","day")
    )

