library(tidyverse)
#1
v <- read.csv("C:/Users/madin/Dropbox/Studium/unterricht folien/3 Semester/Statistic/testing_covid.xls.csv")
head(v)
view(v)

#3

drei <- v %>% filter(ï..country == "Germany") %>% 
              separate(year_week , into = c("year", "Week"), sep = "-" ) %>% 
              select(Week,tests_done )
drei
plot(c(1:57), drei$tests_done, type ="l")

#4
vier<- v %>%   filter(ï..country %in% c("Germany", "Austria", "France","Italy")) %>% 
                filter(year_week == "2020-W50"|year_week == "2020-W51"|
                       year_week == "2020-W52"|year_week == "2020-W53") %>% 
                filter(level == "national") %>% 
               select(ï..country, year_week, level, positivity_rate)
vier

pie(vier$positivity_rate,labels = vier$ï..country )

#5

f <- v %>%  filter(ï..country %in% c("Germany", "France") ) %>% 
            select(testing_rate, ï..country)
f
min(f$testing_rate)
max(f$testing_rate)
quantile(f$testing_rate, probs = c(0.25,0.5,0.75))

#### Nur für Germany 


ger <- v %>%  filter(ï..country %in% c("Germany") ) %>% 
  select(testing_rate, ï..country )
min(ger$testing_rate)
max(ger$testing_rate)
quantile(ger$testing_rate, probs = c(0.25,0.5,0.75))



Fra <- v %>%  filter(ï..country %in% c("France") ) %>% 
  select(testing_rate, ï..country)
min(Fra$testing_rate)
max(Fra$testing_rate)
quantile(Fra$testing_rate, probs = c(0.25,0.5,0.75))
















