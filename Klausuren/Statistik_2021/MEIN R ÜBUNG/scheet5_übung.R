library(tidyverse)


#aufgabe 1#

x <- c(2,6,3,4,5)
y <-c(3,7,4,7,6)
x 
y
#scatterplot
par(mfrow = c(1,1))#mfrow wie viele werte sollen auf x und y achse 1 ist standard
plot(x,y,main = "Scatterplot", xlim =c(0,7), ylim = c(0,8))
#a
cov(x,y)
#b
cor(x,y)
#c
#defiiert lm (x^y)
lm (x~y)

#regression line: x=a+by
abline(lm(x ~ y),col="red")

#d
#regression line: y=a+bx
abline(lm(y ~ x),col="blue")





#aufg2 scatterplot
xi <- c(10,9,9,11,10,10,6,10,8,12,9,4,12)
yi <- c(5,5,4,6,7,5,3,4,5,7,4,2,8)


plot (xi,yi, main = "time spent in exercise so", 
      ylim = c(0,8), xlim = c(5,13)
      )
#c
cov(xi,yi)
cor(xi,yi)

#d

lm (xi~yi)
abline (lm(yi ~ xi),col="blue")

#e
a<-lm (yi~xi)$coefficient[1]

b<-lm (yi~xi)$coefficient[2]

c<-a+b*8


#g
cor(xi,yi)^2

#h
lm (yi~xi)


xi<-c(xi,20)#wert in xi anfügen
yi<-c(yi,0)#wert in yi anfügen
abline (lm(yi ~ xi),col="red")

#4.1
coffee <- tibble(
  
  c(1:5),
  x=c(3,8,7,9,5),
  y=c(6,7,10,8,4)
  
)#spearmans

coffee <- coffee%>% mutate(R_X = rank(x))%>% #neue spalte einfuegen
                    mutate (R_Y = rank (y)) %>%
                      #mutate(d=c(10:1)) spalete einfügen


cor(x=coffee$x,y=coffee$y,method = "spearman")


coffee <- coffee %>% mutate(Rdesc_x = rank(-x))%>%
                      mutate ( Rdesc_y = rank(-y))


#aufgbe 4

sstudent<-tibble(attaendance=c(rep("over 70%",50),rep("30%- 70%",30),
                               rep("under 30%",20)),
                 course_result=c(rep("pass",40),rep("fail",10),
                                 rep("pass",20),rep("fail",10),
                                 rep("pass",10),rep("fail",10)
                               ))
chi_test<-sstudent %>% table() %>% chisq.test()
chi<-chi_test$statistic

c<-(chi/(chi+length(sstudent$attaendance)))^0.5
length(sstudent)

e<-min(length(sstudent),#anzahl der spalten 2 spalten
       length(sstudent$attaendance)) #Anzahl der Reihen 100
corr<-((e/(e-1)))^0.5*c#fprmel von corr




cong_sttudent<-table(sttudent)
addmargins(sttudent$pass)


#aufgabe 3a

raw_data<-tibble(class=c(rep("first",325),rep("second",285),rep("third",706),rep("staff",885)),
                 state=c(rep("rescude",203),rep("not.rescude",122),
                         rep("rescude",118),rep("not.rescude",167),
                         rep("rescude",178),rep("not.rescude",528),
                         rep("rescude",212),rep("not.rescude",673)))
raw_data_tab<-raw_data %>%table()

addmargins(raw_data_tab) #fügt automatisch die zeile Sum
#b

raw_data_tab/rowSums(raw_data_tab)#rowsums funktion für summen der reihe

#c)
chi_test2<-raw_data_tab%>%chisq.test()
chi2<-chi_test2$statistic          #x2 berechenen
c2<-(chi2/(chi2+length(raw_data$class)))^0.5 #c berechnen
e2<-min(length(raw_data),length(raw_data$class))
corr2<-(e2/(e2-1))^0.5*c2



#d
raw_data%>%unite(raw.data,c(first,second))





MY<- LETTERS [1:6]
MY

Sulem <- sample(1:4, 7)
Sulem









