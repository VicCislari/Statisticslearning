library(tidyverse)
library(psych)

####Aufgabe 1
result <- tibble( party =c ("CDU", "SPD", "AFD", "FDP", "DIE Linke", "GRÜNE", "CSU", "Others"),
                  Results_2013 =c (26.8, 20.5,12.6,10.7,9.2, 8.9, 6.2,5.0),
                  Results_2017 = c(34.1,25.7,4.7,4.8,8.6,8.4,7.4,6.2)
               
  
)
result
par(mfrow = c(1,1))
pie(result$Results_2013, main = "Result 2013", labels = paste(result$party,"(",result$Results_2013,"%)"))

barplot(result$Results_2017,main = "Result_2017", names.arg = result$party)
barplot(result$Results_2013,main = "Result_2013", names.arg = result$party)

################ Aufgabe 2
times<-c(568, 568, 581, 640, 641, 645, 657, 673, 696, 703, 720, 728, 729, 777, 808,
         824, 825, 865, 875, 1007)

a<- table( times)
a  
cumulative <- cumsum((prop.table(table( times))*100))


non_player<-c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2)
beginner<-c(32.5,37.1,39.1,40.5,45.5,51.3,52.6,55.7,55.9,57.7)

tour<-c(40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3)


##################### Aufgbae 6

obversations <- tibble( num = c(1:8),
                        abs_fre = c (5,4,1,7,2,3,1,2)
)  
a <- rep(obversations$num,obversations$abs_fre)
a

harmonic.mean(a)
geometric.mean(a)
mean(a,trim = 0.2)
########################AUfgbae 8

sample <- c(3,7,2,5,6,10,6,3,6,5)
sample

mean(sample)
quantile(sample, probs = c(0.25, 0.5,0.75))
var(sample)
geometric.mean(sample)
harmonic.mean(sample)
mean(sample, trim = 0.1)

################## Aufgbae 9

# #e <- tibble( non_player = c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2),
#              beginner =c(32.5,37.1,39.1,40.5,45.5,51.3,52.6,55.7,55.9,57.7),
#              tour = c(40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3)
# 
# )


p<- tibble(type= c (rep ("non_players ",10),rep ("beginners ",10),
                    rep ("tourn_pla",10)),
           res = c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2,
                   32.5,37.1 ,39.1 ,40.5 ,45.5,51.3,52.6,55.7,55.9 ,57.7,
                   40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3)
)
p

p %>% group_by(type) %>% summarise(mean = mean(res),
                                median = median(res),
                                min    = min(res),
                                max    = max(res),
                                quantile1 = quantile(res, 0.25, type = 1),
                                quantile2 = quantile(res, 0.5,type = 1),
                                quantile3 = quantile(res, 0,75, type = 1),
                                interquantile = quantile3 -quantile1) 

p
boxplot(non_player,beginner,tour)

boxplot(p)
#################### Aufgabe 10

distance<-c(12.5, 29.9, 14.8, 18.7, 7.6, 16.2, 16.5, 27.4, 12.1, 17.5)
altitude<-c(342, 1245, 502, 555, 398, 670, 796, 912, 238, 466)
#######   10a

mean (distance)
mean (altitude)
median(distance)
median(altitude)

######## 10 b

q1 <- quantile (distance, probs = c(0.25, 0.75), type = 1)
q2 <- quantile(altitude, probs = c(0.25, 0.75), type = 1)

######## 10c 
sd (distance)
sd (altitude)

interquartile1 <-  quantile(distance, probs=c(0.75))-quantile(distance,probs=c(0.25))
interquartile2 <- quantile (altitude, probs = c(0.75)) - quantile (altitude, probs = c(0.25))
interquartile1;interquartile2

##### 10d

boxplot(distance,altitude)

################# Aufgabe 11
library(ggplot2)

head(mpg)
#### 11a
?mpg()
###11b

mpg 
 mpg %>% select (displ, hwy) %>%
     mutate(displ_class = cut(displ,breaks =c(1,3,5,8), labels = c("low", "mdium", "big") )) %>% 
   group_by(displ_class)
a   

########11 c
   
   mpg <- mpg %>% group_by(displ) %>% 
                summarise(mean = mean(hwy), min=min(hwy), max=max(hwy),
                          q1 = quantile (hwy, probs = 0.25, type=2),
                          q2 = quantile (hwy, probs = 0.5, type = 2),
                          q3 = quantile (hwy, probs = 0.75, type = 2),
                       all = quantile (hwy, probs = c(0.25,0.5,0.75), type = 2 ))
   
   
   mpg<- mpg %>% 
     group_by(displ_class) %>% 
     summarise(mean = mean(hwy), min=min(hwy), max=max(hwy),
               q1 = quantile (hwy, probs = 0.25, type=1),
               q2 = quantile (hwy, probs = 0.5, type = 1),
               q3 = quantile (hwy, probs = 0.75, type = 1),
               all = quantile (hwy, probs = c(0.25,0.5,0.75), type = 1 ))
  
   
   
   ##################
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
            
    





























