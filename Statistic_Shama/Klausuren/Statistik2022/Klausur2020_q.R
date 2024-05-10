library(tidyverse)




a<- read.csv("C:/Users/madin/Dropbox/Studium/unterricht folien/3 Semester/Statistic/Statistik_2021/Testingg_covid.csv")
head(a)
#3
drei<- a %>% filter(country == "Germany") %>%
  select(country, year_week,tests_done) %>% 
  separate(col = year_week, into = c("year", "week"), sep = "-" ) %>% 
  select(country,tests_done,week ) ;drei

plot(drei$tests_done, type ="l",col="darkred")
#4

vier <- a %>% filter(country %in% c("Germany","Austria", "France","Italy"), 
                     year_week =="2020-W20", level == "national" ) %>% 
              select(country, year_week, level)
vier
#5
fünf <- a %>%   group_by(testing_rate) %>%
                summarise(minimum = min(country == "Germany"),
                          maximum = max(country == "Germany"))

fünf

#5
fünf <- a %>% filter %>%   group_by(testing_rate) %>%
 summarise(minimum = min(country="Germany" )  ,
           maximum = max(country="Germany"))

;fünf
            # minimum = min(country == "France"),
            # maximum = max(country == "France"),
            # q1      = quantile (country == "Germany", 0.25, type = 1),
            # q2      = quantile (country == "Germany", 0.5, type = 1),
            # q3      = quantile (country == "Germany", 0.75, type = 1),
            # q1      = quantile (country == "France", 0.25, type = 1),
            # q2      = quantile (country == "France", 0.5, type = 1),
            # q3      = quantile (country == "France", 0.75, type = 1))
            # 
            fünf

f <- a %>%  group_by(testing_rate) %>% 
           quantile(country=="Germany", probs=c(0.25,0.5,0,75))















