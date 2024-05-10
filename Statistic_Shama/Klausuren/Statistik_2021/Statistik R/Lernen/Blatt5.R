library(tidyverse)
library(ggplot2)

x<-c(2,6,3,4,5)
y<-c(3,7,4,7,6)

#covariance
sxy<-((1/(length(x)-1))*sum((x-mean(x))*(y-mean(y))))
cov(x,y)


#coefficient of correlation
sx<-((1/(length(x)))*sum((x-mean(x))^2))^0.5
sy<-((1/(length(y)))*sum((x-mean(y))^2))^0.5
rxy<-sxy/(sx*sy)

cor(x,y)

lm(y~x)

xy<- tibble(x,y)
plot(xy)

#FALSCH?
#regression line: x=a+bx
abline(lm(xy$x ~ xy$y),col="red")

#regression line: y=a+bx
abline(lm(xy$y ~ xy$x),col="blue")


#####################
#AUFGABE2#
xi<-c(10,9,9,11,10,10,6,10,8,12,9,4,12)
yi<-c(5,5,4,6,7,5,3,4,5,7,4,2,8)

xi<-add_row(8)

#a)
plot(xi,yi)

#b)
cor(xi,yi) #positive linear relationship

#c)
cov(xi,yi)
cor(xi,yi)

#d)
reg<-lm(yi~xi)
abline(reg,col="red")

#e) y=a+bx !!! funktion merken
a<-reg$coefficients[1]
b<-reg$coefficients[2]

score_8<-a+b*5

#f)

#g)
proportion_of_variation<-(cor(x,y))^2

#h)


#########################################
#Descriptive Statistics - Contingency Tables##

#Aufgabe1

dat<-tibble(cafe=c(1:5),x=c(3,8,7,9,5),y=c(6,7,10,8,4))

dat<- dat %>% mutate(R_x=rank(x),R_y=rank(y))

cor(dat$R_x,dat$R_y,method = "spearman")


#b)
dat<- dat %>% mutate(R_xdesc=rank(-x),R_ydesc=rank(-y))
cor(dat$R_xdesc,dat$R_ydesc,method = "spearman")


#c)
dat2<-tibble(quality=c("good","bad"),
             journalist1=c(2,3),
             journalist2=c(1,4))
#journalist1 good(2/5)



#Aufgabe3

#raw_data<- tibble(class=c("first","second","third","staff"),
 #                 rescude=c(203,118,178,212),not.rescude=c(122,167,528,673))
#a)
raw_data<-tibble(class=c(rep("first",325),rep("second",285),rep("third",706),rep("staff",885)),
            state=c(rep("rescude",203),rep("not.rescude",122),
                    rep("rescude",118),rep("not.rescude",167),
                    rep("rescude",178),rep("not.rescude",528),
                    rep("rescude",212),rep("not.rescude",673)))
cong_dat<-table(raw_data)

addmargins(cong_dat) #sum von zeilen und reihen hinzufügen in einer e

#b)
cong_dat2<-cong_dat/rowSums(cong_dat) #positive linear realtionship => there is a relationship?

#c)
chi_test<-raw_data %>% table() %>% chisq.test()
chi_test$statistic

e<-min(length(raw_data),length(raw_data$class))

c<-(chi_test/(chi_test/length(raw_data$class)))^0.5

corr<-c*(e/(e-1))^0.5

#d)
raw_dat2<-raw_data %>% mutate("class"=if_else((raw_data$class=="first" | raw_data$class=="second"),"first+second","third+staff")) 
#Variablenname "class" vor if_else muss mit raw_data variablenname class übereinstimmen

chi_test2<-raw_dat2%>%table() %>% chisq.test()
chi_test2$statistic
chi_test2$expected

e<-min(length(raw_dat2),length(raw_dat2$class))

c<-(chi_test2/(chi_test2/length(raw_dat2$class)))^0.5

corr<-c*(e/(e-1))^0.5

#c



raw_dat2<- subset(raw_data,raw_data$class=="first" | raw_data$class=="second") %>% table()

raw_dat<- which(raw_data$class=="first")
marginSums(raw_dat)
