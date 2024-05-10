library(tidyverse)
library(ggplot2)
party <- c('CDU','SPD','AfD','FDP','D_L',"GR",'CSU',"Other")
result13<-c(26.8,20.5,12.6,10.7,9.2,8.9,6.2,5.0)
result17<-c(34.1,25.7,4.7,4.8,8.6,8.4,7.4,6.2)

dat<-tibble( party ,result13,result17)

barplot(dat$result13, xlab = "party",names.arg = party,ylim=c(0,30),
        ylab ="prozent", main = "national elections in Germany in 2013 ")

barplot(dat$result17, xlab = "party",names.arg = party,ylim=c(0,40),
        ylab ="proznt", main = "national elections in Germany in 2017 ")

par ( mar=c (2,2,2,2 ) , mfrow=c ( 1 , 1 ) , cex = 0.7) #Einstellung der Schriftart(cex), und der Anzahl der Abbildungen pro Seite(mfrow),Anzeigegröße(mar)
pie ( result17 , labels = paste( party, " ( ", result17 , " ) "),radius=0.5 )
pie ( result13 , labels = paste( party, " ( ", result13 , " ) "),radius=0.5 ) #paste wandelt Zahlen(bzw.alles) in einen String um

dif<- result17-result13  # differenze um zu vergleichen

dat<- dat%>% mutate( dif)

barplot(dif, xlab = "party",names.arg = party,ylim = c(-10,10),
        ylab ="prozent", main = "national elections in Germany in 2017 compared to 2013(in prozent) ")


#Aufgabe2

times<-c(568, 577, 581, 640, 641, 645, 657, 673, 696, 703, 720, 728, 729, 777, 808,
         824, 825, 865, 875, 1007)
abs<-table(times)  #gibt die Häufigkeiten von times an

tib<- tibble(times,abs)
tib %>% mutate(rel=cumsum(tib$abs)/length(tib$times))%>% select(times,rel)

#Alternative
tib2<-tibble(times) 
H <- ecdf(tib2$times)  #Funktion bei der man Vektoren einsetzten kann und die cumulative Frequence berechenen kann
#H(tib$times)


tib2<- tib2 %>% select(times) %>% mutate(H_x=H(times)) %>% unique() %>% arrange(times) #unique => Daten die beim Vektor gleich sind werden eliminiert, arrange sortiert von klein nach groß

plot.ecdf(tib2$times, ylab="H(x)",main="empirical cumulative distribution function")


#Aufgabe2b)

#less equal 800
H(800)

#greater than 725
1-H(725)

#greater than 642 and less equal 777
H(777)-H(642)

#equal 696
H(696)-H(695)

#Aufgabe2c)

intervall<-c(500,600,700,800,900,1000,1100)
#################################################################### FEHLER
tib2<-cut(times, breaks =intervall    )
#timer.cut<-cut(tib2$times,intervall) #Spalte für Intervalle gebildet
######################################################################


#tib2$timer.freq=table(tib2)

tib2 <- tib2 %>% count(timer.cut)  #tibbble mit Intervallen
tib2 %>% mutate(rel= n/sum(n))

hist(times, breaks = intervall, xlab = "time", ylab ="")

#Aufgabe2d)

intervall2=c(500,600,900,1000,1100) 

tib4<- tibble(int=cut(tib2$times,breaks=intervall2,labels = intervall2[-1] )) ##Standard->auswendig

tib4<-tib4 %>% count(tib4$int) %>% mutate(rel=n/sum(n))  ##Standard->auswendig

hist(times, xlab = "Intervall", main = "Histogram")

hist(times,intervall2,xlab="times")



bounds <- c(500,600,700,800,900,1000,1100)

#cut(times, breaks = bounds    )

times_cut <- cut(times, breaks = bounds,
                 # labels denotes the names of values
                 # default: classes like (500,60], ...
                 # here: value = upper bound of the class
                 labels = bounds[-1]) # leave the first value

cut(times, breaks = bounds) # labels are the classes (a,b]

df_cut <-
  tibble(upper_bound = times_cut) %>%
  group_by(upper_bound) %>%
  mutate(n = n(),
         rel = n / length(times)) %>%
  ungroup() %>%
  unique() %>%  # remove multiple entries
  mutate(cum_rel_freq = cumsum(rel))
df_cut



##############################################
#Descriptive Statistics - Measures
########################################
#Aufgabe1


#Aufgabe2
# year1: 1000*1,13, year2: year1*1,22,year3: year2*1,12...
# year1: 1000*1,13 year2: 1000*1,13*1,22 year3:

returns<- c(1.13,1.22,1.12,0.95,0.87)

values<-1000*cumprod(returns) #cumprod um die einzelnen Werte/Zwischenschritte zu bekommen


#geometric mean 5wurzel(pro(returns))
#
values2<-(prod(returns)^0.2)
values3<-( prod (1+ returns)^0.2 -1)*100

#c) ????

#Aufgabe5

number<-c(1,2,3,4,5,6,7,8)
absfreq<-c(5,4,1,7,2,3,1,2)
obs<-rep(number,absfreq)

#mean
mean(obs)

#geometric mean
prod(x)^(1/length(x))

#harmonic means
length(x)/sum(1/x)

#20%mean
mean(x,trim=0.1) #warum 0.1 nicht 0.2

#Aufgabe7

y<-c(3, 7, 2, 5, 6, 10, 6, 3, 6, 5)
quantile(y,probs=0.25)
quantile(y,probs=0.5)
quantile(y,probs=0.75)

#gerundete Werte, die man auch in y finden kann
quantile(y,type=1)

#ungerundete werte, wert kann man so nicht in y finden
quantile(y,type=7)

#default ist type=7
quantile(y)

#kp was 10% im trim ist ob 0,1 oder 0,2
mean(y,trim=0.1)



#Aufgabe8

non_players<- c(22.1,22.3,26.2,29.6, 31.7,33.5,38.9,39.7,43.2,43.2)
beginners<-c(32.5,37.1,39.1,40.5,45.5,51.3,52.6,55.7,55.9,57.7)
tournament_players<-c(40.1,45.6,51.2,56.4,58.1,71.1,74.9,75.9,80.3,85.3)

chess<-tibble(type=c(rep("non_players",10),rep("beginners",10),rep("tournament_players",10)),
              player=c(non_players,beginners,tournament_players))

#non_players
non_playersData<- c(mean(non_players),median(non_players),min(non_players),
                         max(non_players),quantile(non_players,type=1),var(non_players))


beginnerData<- c(mean(non_players),median(non_players),min(non_players),
                    max(non_players),quantile(non_players,type=1),var(beginners))
boxplot(non_players,beginners,tournament_players)


boxplot(non_players)

#Aufgabe9
distance<-c(12.5, 29.9, 14.8, 18.7, 7.6, 16.2, 16.5, 27.4, 12.1, 17.5)
altitude<-c(342, 1245, 502, 555, 398, 670, 796, 912, 238, 466)

mean(distance)
mean(altitude)

median(distance)
median(altitude)

quantile(distance,0.25,type=1)  #Type1 rundet nicht 
quantile(distance,0.75,type=1)

quantile(altitude,0.25,type=1)
quantile(altitude,0.75,type=1)

#c) interquartile
interquantileAltitude<-quantile(altitude,0.75,type=1)-quantile(altitude,0.25,type = 1)

interquantileDistance<-quantile(distance,0.75,type=1)-quantile(distance,0.25,type=1)

#standard deviation
StandardDeviationDistance<-((1/(length(distance)-1))*sum((x-mean(distance))^2))^0.5

StandardDeviationAltitude<-((1/(length(altitude)-1))*sum((x-mean(altitude))^2))^0.5

#compare<-table(interquantileAltitude,interquantileDistance,StandardDeviationAltitude,StandardDeviationDistance)

#d)
boxplot(distance,altitude)

#e)
i<-c(5,15,20,30)
i_cut<- cut(distance,breaks=i)
distance_cut<-tibble(upper_bounds=i_cut)%>%
              group_by(upper_bounds)%>%
              mutate(n=n(),rel=n/length(distance))%>%
              ungroup()%>%
              unique()%>%
              mutate(cum_rel_freq=cumsum(rel))

#weighted mean
weighted.mean(distance)
weighted.mean(c(10,17.5,25),c(4/10,4/10,2/10))  #?? Lösungen

##Aufgabe10
#a)
library(tidyverse)
?mpg()

#b)

economy_data<-tibble(mpg$displ,mpg$hwy)

intervall3<-c(1,3,5,8)
intervall3_cut<-cut(mpg$displ,breaks = intervall3,labels = intervall3[-1])
economy_data_cut<-tibble(upper_bounds=intervall3_cut)%>%
  mutate(group= ifelse(upper_bounds==3,"low",
                       ifelse(upper_bounds==5,"medium",
                              ifelse(upper_bounds==8,"big","Error"))))
economy_data <- economy_data %>% mutate(displ_class=economy_data_cut$group)


#calculate the mean of low
#economy_data <- economy_data %>%group_by(economy_data$`mpg$displ`)%>% mutate(mean_hwy=c(mean(economy_data$`mpg$hwy`)))
#,min_hwy=(min(economy_data$`mpg$hwy`)),max_hwy=(max(economy_data$`mpg$hwy`)))
"
economy_data <- economy_data %>%
  group_by(mpg$displ)%>%
  mutate(n=n(),rel=economy_data$`mpg$hwy`/n)%>%
  ungroup()%>%
  unique()
 "


e<-table(economy_data$`mpg$displ`)

#%>%
 # group_by(upper_bounds) 

