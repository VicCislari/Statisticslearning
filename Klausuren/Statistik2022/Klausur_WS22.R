library(tidyverse)
library(psych)

x= read.csv("C:/Users/madin/Dropbox/Studium/unterricht folien/3 Semester/Statistic/melanoma (1).csv")
View(x)
head(x)

x<- x %>% mutate(sex_x= case_when(sex == 1 ~ "Male",
                              sex == 0 ~ "Female"),
             status_s= case_when(status == 1 ~"Dead with Melanoma",
                                 status == 2 ~"Alive",
                                 status == 3 ~"Dead without Melanoma"),
              ulcer_u =case_when(ulcer== 1 ~"Presens",
                                 ulcer== 0 ~"Obsense"),
             live.status = case_when( status == 2 ~"Alive",
                                      status == 3 ~"Dead " ))
head(x)


table(x$sex,x$live.status) %>% addmargins()



x %>% group_by(sex) %>% summarise(min = min(age),
                                  max = max(age),
                                  mean = mean(age),
                                  q1   = quantile(age, 0.25, type = 1),
                                  q2   = quantile(age, 0.5, type = 1),
                                  q3   = quantile(age, 0.75, type = 1))




boxplot(x$age~x$sex, col = c("darkred"))



add_data_melanoma_1_ <- read.csv("C:/Users/madin/Dropbox/Studium/unterricht folien/3 Semester/Statistic/add.data.melanoma (1).csv")
View(add_data_melanoma_1_)

head(add_data_melanoma_1_)
add_data_melanoma_1_ %>% separate(sex_age_year, into = c("sex","age","year"),sep = "/" )
