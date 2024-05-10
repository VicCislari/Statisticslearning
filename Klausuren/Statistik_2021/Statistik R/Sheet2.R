student2 <- tibble(
  name = rep(c("Adam", "Bernd", "Christian", "Doris"), each = 2),
  type = rep(c("height", "weight"), 4),
  measure = c(1.83, 81, 1.75, 71, 1.69, 55, 1.57, 62))

#Aufgabe2
5+3 %>% sqrt %>% log %>% sin

v1<-c(seq(from=0.5,to=5,by=0.5))

v1 %>% sqrt %>% log %>% sin

#Aufgabe3          

df <-tibble(id=1:10,
            sex=sample(x=c("f","m"),size=10 ,replace = TRUE) ,
            age = round( runif ( 10 , 20 , 35 ) ) ,
            score1 = round( runif ( 10 , 0 , 25 ) )
)

#select data of all men

filter(df,sex=="m")

df %>% filter(sex=="m")

df<-df %>% add_row(id=11, sex="m",age=25,  score1=4)

df<-df %>% add_column(score2=round(sample(0:25,11)),score3=round(sample(0:25,11)))


#sumscore
df <- df %>% mutate(sumscore=score1+score2+score3)

within(df,sumscore<-score1+score2+score3)

#

 df<-df %>% mutate(df,grade= ifelse(sumscore<=37, 5, 
                          ifelse(sumscore>37 & sumscore<=45, 4,
                                 ifelse(sumscore>45 & sumscore<=55,3,
                                        ifelse(sumscore>55 & sumscore<=65,2,
                                               ifelse(sumscore>=65,1,NA))))))


 df[order(filter(df,grade<5)$sex),]
 
 ###########################################################
 #Aufgabe4
 
 library(tidyverse)
 library(nycflights13)
 help(nycflights13) 
name(nycflights13) 
names(flights) 
 
flights %>% filter(arr_delay>120)

flights %>% filter(arr_delay>120 & dep_delay<=0)

flights %>% filter( carrier %in% c("UA","AA","DL") & arr_delay<=0)

flights %>% filter ( carrier %in% c("UA","AA","DL") & month==5 &arr_delay>300) %>% select(carrier,flight) %>% arrange(carrier,flight)%>% unique()

#Exchange the values of departure time and arrvial time in minute after midnight.

flights %>% mutate(dep_time,dep_time=dep_time%/%100*60 + dep_time%%100)

flights %>% add_column(speed=air_time)

flights<- flights %>% mutate(speed=distance/(air_time/60)) 



flights %>% mutate(speed=distance/(air_time/60))  %>% select(carrier,flight,speed)%>% arrange(desc(speed)) %>% top_n(10,speed)


count(flights,arr_delay<10)
flights %>% filter(arr_delay<10)
flights %>% filter(arr_delay<10) %>% group_by(carrier)
flights %>% filter(arr_delay<10) %>% group_by(carrier) %>% summarise(n=count(flights,arr_delay<10))

#flights %>% filter(arr_delay<10) %>% count(flight,carrier) %>% mutate(ratio=(n / (flights %>% count(flight,carrier


flights %>% filter(!is.na(arr_delay)) %>%
  mutate(bool_delay=if_else(arr_delay>10,1,0)) %>% 
  group_by(carrier)%>% 
  mutate(total_flights=n()) %>% 
  mutate(num_delay=sum(bool_delay)) %>% 
  mutate(ratio=num_delay/total_flights)%>% 
  select(carrier,ratio,total_flights) %>% 
  unique()



flights %>%filter(!is.na(arr_delay)) %>% 
  mutate(bool_delay=if_else(arr_delay>10,1,0)) %>%
  group_by(carrier,month)%>% 
  mutate(total_flights=n()) %>%
  mutate(num_delay=sum(bool_delay)) %>% 
  mutate(ratio=num_delay/total_flights)%>% 
  select(month,carrier,total_flights,ratio)%>% 
  group_by(month)%>% 
   filter(ratio==max(ratio))%>%    #????????????????????
  unique()

flights %>% mutate(bool_delay=if_else(!is.na(arr_delay),1,0),bool_depdelay=if_else((dep_delay<=5 && dep_delay>=-5),1,0),bool)



                                                 