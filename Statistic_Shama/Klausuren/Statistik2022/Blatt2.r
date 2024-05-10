library(tidyverse)
library(psych)
library(ggplot2)
library(nycflights13)


student1 <- tibble(
  student = c("Adam","Bernd","Christian","Doris"),
  algebra = c(NA, 5, 3, 4),
  analysis = c(2, NA, 1,3),
  diskrete.math = c(3,NA,2,4),
)
#Lösung
student1 %>% gather("algebra","analysis", "diskrete.math",key ="Modul" ,value ="Mark" )

##############################
student2 <- tibble(
  name = rep(c("Adam", "Bernd", "Christian", "Doris"), each = 2),
  type = rep(c("height", "weight"), 4),
  measure = c(1.83, 81, 1.75, 71, 1.69, 55, 1.57, 62))

#lösung
student2 %>% spread (key=type ,value =measure )

###########################
student3 <- tibble(
  name = c("Adam", "Bernd", "Christian", "Doris"),
  ratio = c("81/1.83", "71/1.75", "55/1.69", "62/1.57"))
##Lösung
student3 %>% separate(col = ratio, into = c("height","weight"),sep = "/")

########## Aufgbbe 2
#2a
s<- sin(log(sqrt(5+3)))
s
#2b
v <- seq(0.5,5, by = 0.5)
v
v <- round (sum(log(v**0.5)),2)
v

################Aufgabe 3

df <- tibble( id = c( 1:10),
              sex = sample(c("f","m"), size =10,replace = TRUE),
              age = round(runif(10,20,35)), #10 Studenten
              score1 = round(runif(10,0,25))
              
)


df
#3a
df %>%  filter(sex == "m") #oder subset
#3b
 df <-  add_row(df,id = 11,sex="m",age =25,score1 = 4)
#3c
 
 df <- df %>% mutate(score2 =  round (runif(11,0,25)) ,
               score3 = round (runif (11,0,25)),
               scoresum    = (score1+score2+score3), # 3d
               grade  =  case_when(                  #3e
                 
                 scoresum <= 37 ~ 5,
                 scoresum >  37  & scoresum <= 45 ~ 4,
                 scoresum >  45  & scoresum <= 55 ~ 3,
                 scoresum >  55  & scoresum <= 65 ~ 2,
                 scoresum >= 65 ~ 1
                    )
                )   
 
 #3f
 
 df %>% select(id, sex, grade) %>%
              subset(grade < 5  ) %>% 
              arrange(sex)
 #3g
 
 
 df %>% group_by(sex) %>% summarise( mean    = mean   (scoresum),
                                     minimum = min    (scoresum),
                                     maximum = max    (scoresum),
                                     median  = median (scoresum)
                                 )
 
###########################  Aufgabe 4
 
 #4a --------------> library (nycflights13)
 
 ?flights()
 head(flights)# show the table in console
 view(flights) # show the complete table in new Tab
#4b
 a <- flights
 a
 del<-a %>% subset(arr_delay > 120)
        del
#4c
 del_no <- a %>% subset(arr_delay > 120 & dep_delay <= 0)
     del_no
#4d
no <- a %>% filter(carrier == "UA" | carrier == "AA" |carrier == "DL") %>%
             filter(arr_delay <= 0)
     no
#4e
e <- a %>% filter(carrier %in% c("UA","AA","DL") & month == 5,
                  arr_delay > 300) %>%
          arrange(carrier, flight) %>%
         select(carrier,flight)
e
# #-------------------------------------------------------    
            #4f --------------------> ???????
#-------------------------------------------------------     
    
#4G
     
G <-   a %>% mutate(speed =  distance / air_time * 60) %>% 
             select (carrier,flight,speed) %>% 
             arrange(desc (speed)) %>% 
             top_n(10,speed)
 
       G
#-------------------------------
        # 4H ----------> ?????
#-----------------------------------        

     
   
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
 
 
 
 
 
 
 
 
 
 
 
 
  
 
 
 
 
 
 
 
 



































