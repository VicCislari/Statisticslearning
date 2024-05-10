
library(ggplot2)
  party = c("CDU", "SPD", "AfD", "FDP","DIE LINKE","GRUENE","CSU","Others")
  Result2013 = c(26.8,20.5,12.6,10.7,9.2,8.9,6.2,5.0)
  Result2017 = c(34.1,25.7,4.7,4.8,8.6,8.4,7.4,6.2)

  lbls <- paste(party, Result2017)
pie(Result2017,labels = lbls)
#alternative

pie (Result2013, main = c("r.2013") ,
     labels=paste(party,"(",Result2017,"%)")  )  



election<-table(party,Result2013,Result2017)

barplot(Result2013,names.arg=party)
barplot(Result2017,names.arg=party)


#Aufgabe2
times<-c(568, 568, 581, 640, 641, 645, 657, 673, 696, 703, 720, 728, 729, 777, 808,
         824, 825, 865, 875, 1007)
tabl<-table(times)
tabl
tib<-tibble(times=(times %>% unique()),n=tabl)
tib
tib<-tib %>% mutate(H=cumsum(n)/length(times))
tib
plot.ecdf(tib$times,ylab= "H")

ggplot(tib,aes(H))+stat_ecdf(geom="point")


#Funktion aus H gemacht man kann jetzt H(600)
ecdf(times) -> H


#Aufgabe2b
#I
tib2<-tibble(times=(times %>% unique()),n=tabl) %>% filter(times<=800)
tib2<-tib2 %>% mutate(H=cumsum(n)/sum(tib2$n))

ggplot(tib2,aes(H))+stat_ecdf(geom="point")

#II
tib3<-tibble(times=(times %>% unique()),n=tabl) %>% filter(times>725)
tib3<-tib3 %>% mutate(H=cumsum(n)/sum(tib3$n))

ggplot(tib3,aes(H))+stat_ecdf(geom="point")

#III
tib4<-tibble(times=(times %>% unique()),n=tabl) %>% filter(times>642 & times<777)
tib4<-tib4 %>% mutate(H=cumsum(n)/sum(tib4$n))

ggplot(tib4,aes(H))+stat_ecdf(geom="point")

#IV

tib5<-tibble(times=(times %>% unique()),n=tabl) %>% filter(times>696)
tib5<-tib5 %>% mutate(H=cumsum(n)/sum(tib5$n))

ggplot(tib5,aes(H))+stat_ecdf(geom="point")

plot.ecdf(tib5$times,ylab= "H")


#Aufgabe2 Discriptive Statistics
AR<-c(0.13,0.22,0.12,-0.05,-0.13)
1000*cumprod(AR+1)
cumprod(AR+1)
prod(1+AR)^0.2
mean(AR)

#Aufgabe5

var<- c(rep(1,5),rep(2,4),rep(3,1),rep(4,7),rep(5,2),rep(6,3),7,rep(8,2))
var

#arithmetic mean
mean(var)
mean(var,trim=0.1) #trimmed 20% mean

#geometric mean
prod(var)^(1/length(var))

#harmonic mean
length(var)/sum(1/var)

#Aufgabe7

#Function
x<-c(3,7,2,5,6,10,6,3,6,5)

#mean
my_mean<-function(x){
  return(sum(x)/length(x))
}


#my_quantile<-function(x,p)

geo_mean<- function(x){
  return(prod(x)^(1/length(x)))
}

harmonic_mean<-function(x){
  
}

quantile(x)



#Aufgabe8

non_player<-c(22.1,22.3,26.2,29.6,31.7,33.5,38.9,39.7,43.2,43.2)
beginner<-c(32.5,37.1,39.1,40.5,45.5,51.3,52.6,55.7,55.9,57.7)

tour<-c(40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3)

##x<-tibble(
##  grp = c(rep("non_player",10), rep("beginner",10),rep("tour",10)),
##  value = c(non_player,beginner,tour)
##)

boxplot(non_player,beginner,tour)

boxplot(t)

#Aufgabe9

#a)
distance<-c(12.5, 29.9, 14.8, 18.7, 7.6, 16.2, 16.5, 27.4, 12.1, 17.5)
altitude<-c(342, 1245, 502, 555, 398, 670, 796, 912, 238, 466)

mean(distance)
mean(altitude)
median(distance)
median(altitude)

#b)
quantile(distance, probs=c(0.25,0.75),type=1)
quantile(altitude, probs=c(0.25,0.75),type=1)

#c)
sd(distance)
sd(altitude)

#interquartile=uooer-lower
interquartile <- quantile(distance, probs=c(0.75))-quantile(distance,probs=c(0.25))

#um zuvergleichen muss man die coeff of varitaion, da es unabängig von einer Maßeinheit ist
sd(distance)/mean(distance)
sd(altitude)/mean(altitude)

#d)
#cant compare the two directly because different units are being used
boxplot(distance)
boxplot(altitude)



#e)
library(tidyverse)

#!!FEHLT!
b<-c(5,15,20,30)
x<- cut(distance,breaks=b)
x
tab<- tibble(x,distance)
tab
n<-count(tab$x)
n
tab<-tab %>% group_by(distance) %>% count() %>% mutate (rel=n/length(distance))
!
#Aufgabe10



#a)
  library(ggplot2)
?mpg()

x<-cut(mpg$displ,breaks=c(3,5,8))
tab2<-mpg%>% select(displ,hwy) %>% mutate(displ_class = cut(displ,breaks=c(1,3,5,8),labels=c("low","medium","big")))

tab2 %>% group_by(displ_class)
