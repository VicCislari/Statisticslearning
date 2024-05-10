library(tidyverse)

student1 <- tibble(
  student=c("Adam","Bernd","Christian","Doris"),
  algebra=c(NA,5,3,4),
  analysis=c(2,NA,1,3),
  diskrete.math=c(3,NA,2,4)
)

student2 <- tibble(
  name=rep(c("Adam","Bernd","Christian","Doris"),each=2),
  type=rep(c("height","weight"),4),
  measure = c(1.83,81,1.75,71,1.68,55,1.57,62)
)

student3 <- tibble(
  name=c("Adam","Bernd","Christian","Doris"),
  ratio= c("81/1.83","71,1.75","55/1.69","62/1.57"))


student1 %>% gather(algebra,analysis,diskrete.math,key="modul",value="grade")

student2%>%spread(type,measure)

student3%>%separate(col=ratio,into=c("x","y"),sep="/")

#Aufgabe2
sin(log((5 + 3)**0.5))
seq(0.5,5,0.5)%>% log()

#Aufgabe3

df <- tibble(id=seq(1,10),
             sex=sample(c("f","m"),10,replace=TRUE),
             age=sample(seq(20,35),10,replace=TRUE),
             score1=sample(seq(0,25),10,replace=TRUE))

df %>% 
  filter(sex=="m")

df<- df %>% add_row(id= 11,
               sex = "m",
               age = 25, 
               score1 = 4)

df<-df %>% add_column(score2=round(runif(11,0,25)),score3=round(runif(11,0,25)))%>% mutate(sumscore=score1+score2+score3)

df<-df %>% mutate(df,grade= ifelse(sumscore<=37, 5, 
                                   ifelse(sumscore>37 & sumscore<=45, 4,
                                          ifelse(sumscore>45 & sumscore<=55,3,
                                                 ifelse(sumscore>55 & sumscore<=65,2,
                                                        ifelse(sumscore>=65,1,NA))))))

df <-
  df %>%
  mutate(score2 = round(runif(10,0,25))) %>%
  mutate(score3 = round(runif(10,0,25))) %>%
  mutate(scoresum = score1+score2+score3) %>%
  mutate(grade = case_when(
    scoresum <= 37 ~ 5,
    scoresum > 37 & scoresum <= 45 ~ 4,
    scoresum > 45 & scoresum <= 55 ~ 3,
    scoresum > 55 & scoresum <= 65 ~ 2,
    scoresum > 65 ~ 1))

df %>% filter(grade<5)%>% arrange(sex)

df%>% group_by(sex) %>% summarise(mean_=mean(sumscore),min_=min(sumscore),median_=median(sumscore))%>%group_by(sex)

#Aufgabe4
library(tidyverse)
library(nycflights13)

flights %>% filter(flights$arr_delay>120)

flights %>% filter (arr_delay>120 & dep_delay==0)

flights %>% filter(carrier %in% c("UA", "AA" , "DL"))

flights %>% filter(carrier %in% c("UA", "AA" , "DL") & month==5 & arr_delay>300) %>% arrange(carrier,flight)%>%select(carrier,flight) %>%unique()


flights %>% mutate(dep_time=(dep_time%/% 100)*60+dep_time%%100)%>% mutate (arr_time=(arr_time%/% 100)*60+arr_time%%100) #hhmm \100 *60(hh*60) + hhmm mod 100(=mm)

flights %>% mutate(avg_speed=distance/air_time/60) %>% select(carrier,avg_speed,flight)%>% arrange(desc(avg_speed))%>%top_n(10,avg_speed)

flights %>% group_by(carrier) %>%summarise(carrier,arr_delay,n()) 