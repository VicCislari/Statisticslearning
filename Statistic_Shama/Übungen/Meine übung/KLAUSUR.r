library(tidyverse)

rawdata <- read.csv("C:/Users/madin/Dropbox/Studium/unterricht folien/3 Semester/Statistic/Statistik_2021/testing_covid.csv")
rawdata2 <- read.csv("C:/Users/madin/Dropbox/Studium/unterricht folien/3 Semester/Statistic/Statistik_2021/testing_covid.csv")
######aufgabe 3
x <-rawdata%>% filter(rawdata$ï..country == "Germany")%>%
            separate(year_week, into = c("year","week"), sep = "-")%>%
            select(ï..country,week)

plot(x$year_week,x$tests_done)

 ggplot(rawdata)+
   geom_line(mapping = aes(x = year_week, y = tests_done))
 
 
########################  aufgabe 4
 
y<- rawdata %>% filter(rawdata$ï..country == "Germany")%>%
            select(ï..country,tests_done,new_cases,year_week)%>%
          subset(year_week > "2020-W42" & year_week < "2020-W54")%>%
                mutate(sum_of_tests = sum(tests_done), 
                   sum_of_newcases= sum(new_cases))%>%
              select(ï..country,sum_of_tests,sum_of_newcases)%>%
            unique()
        
 
 
           rawdata %>% filter(rawdata$ï..country == "Austria")%>%
            select(ï..country,tests_done,new_cases)%>%
             subset(year_week > "2020-W42" & year_week < "2020-W54")%>%
          mutate(sum_of_tests = sum(tests_done), 
          sum_of_newcases= sum(new_cases))%>%
           select(ï..country,sum_of_tests,sum_of_newcases)%>%
         unique()
 

 
           rawdata %>% filter(rawdata$ï..country == "France")%>%
   select(ï..country,tests_done,new_cases)%>%
             subset(year_week > "2020-W42" & year_week < "2020-W54")%>%
   mutate(sum_of_tests = sum(tests_done), 
          sum_of_newcases= sum(new_cases))%>%
   select(ï..country,sum_of_tests,sum_of_newcases)%>%
   unique()
 
 rawdata %>% filter(rawdata$ï..country == "Italy")%>%
   select(ï..country,tests_done,new_cases)%>%
   subset(year_week > "2020-W42" & year_week < "2020-W54")%>%
   mutate(sum_of_tests = sum(tests_done), 
          sum_of_newcases= sum(new_cases))%>%
   select(ï..country,sum_of_tests,sum_of_newcases)%>%
   unique()
 
 raw_data <-separate(raw_data,year_week,into=c("year","week"),sep="-W")
 
 y<-raw_data%>%
   filter((ï..country=="Germany" | ï..country=="France" |
              ï..country=="Austria" | ï..country=="Italy") & year=="2020" & level=="national" & week<=52 &week>=48) %>% 
   group_by(ï..country)%>%
   summarise(sum_testdone=sum(tests_done),sum_newcases=sum(new_cases))####ergebniss alle gleich pie chart geht nicht
              


              pie(c(y$sum_testdone,y$sum_newcases),y("tests done","new cases"))
              
  ########################################aufgabe  5
             
  testing_rate_germany<- rawdata %>% filter(ï..country=="Germany") %>% select(testing_rate)
min(testing_rate_germany)
max(testing_rate_germany)
quantile(testing_rate_germany$testing_rate, c(0.25,0.5,0.75))

ting_rate_france<- rawdata %>% filter(ï..country=="France") %>% select(testing_rate)
min(ting_rate_france)
max(ting_rate_france)
quantile(ting_rate_france$testing_rate, c(0.25,0.5,0.75))

boxplot(ting_rate_france$testing_rate, testing_rate_germany$testing_rate  )



##################aufgabe 6
rawdata <-separate(rawdata,year_week,into=c("year","week"),sep="-W")

xy<- rawdata %>% filter (rawdata$ï..country == "Germany") %>% 
                select(tests_done,week,year) 
xy<-xy %>% add_column(numb=row_number(xy$year))
xy<- xy%>% mutate(number=row_number(xy$year))
            
#numb=0 week10 2020
lm(xy$tests_done~xy$numb)
plot(xy$numb,xy$tests_done)
abline(lm(xy$tests_done~xy$numb))

#predication y=a+b*time
a<-lm(xy$tests_done~xy$numb)$coefficients[1]
b<-lm(xy$tests_done~xy$numb)$coefficients[2]

test_done2021W20<-a+b*64 #640>Wochenanzahl ab Week 10 2020 bis week20 2021



      ################aufgabe6 b

rawdata2%>% filter(level=="national")%>%    
  select(ï..country,tests_done,year_week)%>%
spread(key = year_week, value = tests_done) %>%
  mutate(year = raw_data)
    
              
              
  
  
  
 