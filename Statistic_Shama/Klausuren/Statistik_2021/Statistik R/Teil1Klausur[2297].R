library(tidyverse)
library(readr)

#Aufagbe1
rawdata<-read.csv("C:/Users/madin/Dropbox/Studium/unterricht folien/3 Semester/Statistic/Statistik_2021/testing_covid.csv")
rawdata2 <- read.csv("C:/Users/madin/Dropbox/Studium/unterricht folien/3 Semester/Statistic/Statistik_2021/testing_covid.csv")


#Aufgabe2

scale<- tibble(varibale=c("country","country_code","year_week","level","region","region_name",
                          "new_cases","test_done","population","testing_rate","posivity_rate","testing data source"),
               scale=c("nominal","nominal","interval","nominal","nominal","nominal",
                       "ratio","ratio","ratio","ordinal","ordnial","nominal"))

scale

#Aufagbe3
result_3 <- rawdata %>%
  filter(�..country == "Germany") %>%
  select(tests_done,year_week)

plot(c(1:57),result_3$tests_don, type='l', xaxt='n')

axis(1,at=c(1:57),labels=result_3$week)




########################  aufgabe 4
r4 <- rawdata %>%
  filter(�..country == "Germany" | �..country == "Austria" | �..country == "France" |
            �..country == "Italy") %>%
  filter(year_week == "2020-W50" | year_week == "2020-W51" |
           year_week == "2020-W52" | year_week == "2020-W53") %>%
  filter(level =='national') %>%
  group_by(�..country) %>%
  mutate(sum_test = sum(tests_done), sum_new_cases = sum(new_cases), pos_rate = sum_new_cases/sum_test) %>%
  select(�..country, sum_test, sum_new_cases, pos_rate ) %>%
  unique() 

pie(r4$pos_rate*100, labels= r4$�..country)

r4<-r4 %>%ungroup()%>% mutate(ratio=pos_rate/sum(pos_rate))
pie(r4$ratio, labels= r4$�..country, main = "H")



########################################aufgabe  5

testing_rate_germany<- rawdata %>% filter(�..country=="Germany" & level=="national") %>% select(testing_rate)
min(testing_rate_germany)
max(testing_rate_germany)
quantile(testing_rate_germany$testing_rate, c(0.25,0.5,0.75))

ting_rate_france<- rawdata %>% filter(�..country=="France" & level=="national") %>% select(testing_rate)
min(ting_rate_france)
max(ting_rate_france)
quantile(ting_rate_france$testing_rate, c(0.25,0.5,0.75))

boxplot(ting_rate_france$testing_rate, testing_rate_germany$testing_rate  )



##################aufgabe 6
#rawdata <-separate(rawdata,year_week,into=c("year","week"),sep="-W")

xy<- rawdata %>% filter (rawdata$�..country == "Germany") %>% 
  select(tests_done,year_week) 
xy<-xy %>% add_column(numb=row_number(xy$year_week))

#numb=0 week10 2020
lm(xy$tests_done~xy$numb)
plot(xy$numb,xy$tests_done)
abline(lm(xy$tests_done~xy$numb))

#predication y=a+b*time
a<-lm(xy$tests_done~xy$numb)$coefficients[1]
b<-lm(xy$tests_done~xy$numb)$coefficients[2]

test_done2021W20<-a+b*64 #640>Wochenanzahl ab Week 10 2020 bis week20 2021

#Aufagbe7

#a) tidy


#b)
a7<-rawdata2%>% filter(level=="national")%>%    
  select(�..country,tests_done,year_week)%>%
  group_by(year_week)%>%
  spread(key = year_week, value = tests_done) 

#c) not tidy





