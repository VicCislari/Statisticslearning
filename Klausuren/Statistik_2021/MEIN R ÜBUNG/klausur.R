library(tidyverse)

raw_data <- read.csv("C://Users//madin//Dropbox//Studium//unterricht folien//3 Semester//Statistic//Statistik_2021//testing_covid.csv")

read.csv()


x<- raw_data %>% 
  filter(ï..country=="Germany") %>% 
  select(year_week, tests_done) #gibt die spalte von einer tabelle aus

plot(x$year_week,x$tests_done)

ggplot( raw_data) +
  geom_line(mapping = aes(x$year_week, x$tests_done))

#sum of tests and new cases
y<-raw_data %>% 
  filter(ï..country=="Germany" ) %>%
  select(ï..country, tests_done,new_cases) %>% 
  mutate(sum_testdone=sum(tests_done),sum_newcases=sum(new_cases)) %>%
  select(ï..country, sum_testdone,sum_newcases)%>%
  unique()

y<-add_row(y,

raw_data %>% 
  filter(ï..country=="France" ) %>%
  select(ï..country, tests_done,new_cases) %>% 
  mutate(sum_testdone=sum(tests_done),sum_newcases=sum(new_cases)) %>%
  select(ï..country, sum_testdone,sum_newcases)%>%
  unique())

y<-add_row(y,
        
        raw_data %>% 
          filter(ï..country=="Italy", ) %>%
          select(ï..country, tests_done,new_cases) %>% 
          mutate(sum_testdone=sum(tests_done),sum_newcases=sum(new_cases)) %>%
          select(ï..country, sum_testdone,sum_newcases)%>%
          unique())

y<-add_row(y, raw_data %>% 
          filter(ï..country=="Austria" ) %>%
          select(ï..country, tests_done,new_cases) %>% 
          mutate(sum_testdone=sum(tests_done),sum_newcases=sum(new_cases)) %>%
          select(ï..country, sum_testdone,sum_newcases)%>%
          unique())

raw_data <-separate(raw_data,year_week,into=c("year","week"),sep="-W")

y<-raw_data%>%
filter((ï..country=="Germany" | ï..country=="France" |
            ï..country=="Austria" | ï..country=="Italy") & year=="2020" & level=="national" ) %>% 
  select(ï..country, tests_done,new_cases) %>% 
  mutate(sum_testdone=sum(tests_done),sum_newcases=sum(new_cases)) %>%
  select(sum_testdone,sum_newcases)%>%
  unique()

pie(c(y$sum_testdone,y$sum_newcases),c("tests done","new cases"))


  testing_rate_germany<- raw_data %>% filter(ï..country=="Germany") %>% select(testing_rate)
  min(testing_rate_germany)
  max(testing_rate_germany)
  quantile(testing_rate_germany$testing_rate, c(0.25,0.5,0.75))


testing_rate_france<- raw_data %>% filter(ï..country=="France") %>% select(testing_rate)
min(testing_rate_france)
max(testing_rate_france)
quantile(testing_rate_france$testing_rate, c(0.25,0.5,0.75))

testing_rate_france<- raw_data %>% filter(ï..country=="France") %>% select(testing_rate)
min(testing_rate_germany$testing_rate)
max(testing_rate_germany$testing_rate)
quantile(testing_rate_germany$testing_rate, c(0.25,0.5,0.75))

boxplot(testing_rate_france$testing_rate,testing_rate_germany$testing_rate)