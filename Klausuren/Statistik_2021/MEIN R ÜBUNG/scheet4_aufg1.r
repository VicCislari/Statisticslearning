library(tidyverse)

x <- tibble(
  
  Party = c("CDU","SPD","AfD","FDP", "DIE LINKE", "GRUENE", "CSU", "Others"),
  Results2013=c("26,8%","20,5%","12,6%","10,7%","9,2%","8,9%","6,2%","5,0%"),
  Results2017=c("34,1%","25,7","4,7%","4,8%","8,6%","8,4%","7,4%","6,2%")
  )
x
#code von pie chart NR 1

pie(c(26.8,20.5,12.6,10.7,9.2,8.9,6.2,5.0), main = "Result 2013",
    #labels=c("CDU","SPD","AfD","FDP", "DIE LINKE", "GRUENE", "CSU", "Others"), 
    labels =  paste(x$Party,x$Results2013), #
   border = "black", lty = 30,col = rainbow(24),col.main = "blue",cex = 0.8 #cex= textgröse
   )

#code von barplot NR1 Ich bekomme die % reihe nicht
barplot(c(26.8,20.5,12.6,10.7,9.2,8.9,6.2,5.0), names.arg=paste(x$Party,x$Results2017),xlab = "Party", ylab= "Prozent", col.lab = "blue", 
        cex.names = 0.7,#namen gröse änderung
        cex.axis = 0.8,#prozent änderung
        main = "Result 2017", col.main  ="Black", ylim =c(0,30),
      
            col.axis = "red", col = rainbow(24))
        

#aufgbae 2b

times<-c(568, 577, 581, 640, 641, 645, 657, 673, 696, 703, 720, 728, 729, 777, 808,
         824, 825, 865, 875, 1007)
abs<-table(times)  #gibt die Häufigkeiten von times an

tib<- tibble(times,abs)
tib %>% mutate(rel=cumsum(tib$abs)/length(tib$times))%>% select(times,rel)



#aufgabe 9

library(tidyverse)

times <- C(568, 577, 581, 640, 641, 645, 657, 673, 696, 703, 720, 
           728, 729, 777,777, 808,824, 825, 865, 875, 1007)

a <- table(times)
a
intervall<-c(500,600,700,800,900,1000,1100)
intervall

distance <- c(12.5, 29.9 ,14.8 ,18.7 ,7.6 ,16.2, 16.5 ,27.4, 12.1 ,17.5)
Altitude <- c(342 ,1245, 502, 555 ,398 ,670 ,796 ,912 ,238 ,466)

mean (distance)
mean(Altitude)
median(distance)
median(Altitude)
#prozentansazt rechnung
quantile (distance,0.25,type = 1)
quantile(distance,0.75,type = 1)
quantile (Altitude,0.25,type = 1)
quantile(Altitude,0.75,type = 1)

#9c
interquantileDistance<-quantile(distance,0,75,type = 1)-quantile(distance,0,25,type = 1)
interquantileAltitude<-quantile(Altitude,0.75,type=1)-quantile(Altitude,0.25,type = 1)
#standard
StandardDeviationDistance <- ((1/(length(distance)-1))*sum((x-mean(distance))^2))^0.5
StandardDeviatinAltitude <- ((1/(length(distance)-1))*sum((x-mean(distance))^2))^0.5

#compare in boxplot
boxplot(distance,altitude)

#9e
i <- c(5,15,20,30)
i



##########################################
















