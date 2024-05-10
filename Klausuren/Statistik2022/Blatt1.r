library(tidyverse)
library(ggplot2)

#kommentieren

a <- c("Shama")
b <- c(1:10)
a
###Aufgabe 1a
plus =  c (52.3+74.8+3.17)
#aufgabe 1b
sqrt(144)
#1c
log10(200)*sin(pi/4)

##Aufgabe 1d
cum<-sum(1,3,18,20,2);cum # nur die Summe
cumsum(c(1,3,18,20,2)) # cumulative summe
#####Aufgbae 1e

num <-  sample(0:20,size =10, replace =TRUE)
num  

#oder 
round(runif(10,0,20))

###### Aufgabe 2a
 x <- 5 
 y <- 10
  ###### Aufgabe 2b
x+y
###### Aufgabe 2c
z <- x+y;z
###### Aufgabe 2e
myvec <- c(x,y,z)
myvec
########Aufgabe 2f
min(myvec);min
max(myvec)
mean(myvec)
#################2g
rm(myvec)

########################### Aufgabe 3
##3a
rainfall <- c(0.1 ,0.5, 2.3, 1.1 ,11.3 ,14.7 ,23.4 ,15.7, 0 ,0.9)
rainfall
###3b
mean(rainfall);sd(rainfall)
###3c
cumsum(rainfall);sum(rainfall)
##3d
which.max(rainfall)
max(rainfall)
which.min(rainfall)
###3e 

rainfall <- c(0.1 ,0.5, 2.3, 1.1 ,11.3 ,14.7 ,23.4 ,15.7, 0 ,0.9)


rainfall %>% subset(rainfall > 10)
###3f
mean(rainfall[rainfall >=5])
####3g
rainfall %>% subset(rainfall == 0 | rainfall == 1.1)
###in days  which (rainfall %>% c (0,1.1))
which(rainfall %in% c(0,1.1))

#####################################Aufgbae4
#4a)
cylinders <- c(2.5, 3.4, 4.8, 3.1, 1.7)
diameters <- c(0.7, 0.4, 0.5, 0.5, 0.9)
#4b pi*DIA^2*CYL  

vol <- pi*diameters**2*cylinders
vol
#4c
vol.cm <- (pi*(10*diameters**2)*(10*cylinders))
vol.cm

################ Aufgabe 5
#5a
x <- c(1,2,3,4,5)
y <- c(3,5,7,9)

intersect(x,y) # gleiche werte die beide vectoren enthalten  3 und 5
#5b
setdiff(x,y)#ist in x aber nicht in y
setdiff(y,x)#ist in y aber nicht in x
#5c
union(x,y) # alle werte in x sowohl in y
c(x,y)

##################### Aufgabe 6
#6a

mat1 <- matrix(c(seq(0,18, by = 2), 
                 as.integer(runif(70,0,100))), 
               nrow = 8, ncol = 10, byrow = TRUE)
mat1
#6b
rm<-rowMeans(mat1);rm
sd(rm)
 #6c
mat2 <- mat1[-1,]# komma muss wegen matrix
mat2
cm <- colMeans(mat2)
hist(cm,main ="Shama Gi", col = "darkred", ylab= "enge")

################ Aufgabe 7
#7a

library(ggplot2)
library(tidyverse)
?mpg ()
mpg
view(mpg)# with view(mpg)--> show complete table
head(mpg)# show in consol
#7b + 7c
# 
# str_mpg <- tibble( names = (character()),
#                    type =  (character()),
#                    level = (character()),
#                    dc    = (character()))
#str_mpg
str_mpg <- tibble( name = character(), type = character(), level = character(),
                   dc   = character())
str_mpg
str_mpg1 <- str_mpg %>% 
  add_row(name = "manufacturer", type = "qualitative", level = "nominal", dc = "discrete") %>% 
  add_row(name = "model", type = "qualitative", level = "nominal", dc = "discrete") %>%
  add_row(name = "displ", type = "quantitative", level = "ratio", dc = "continous") %>%
  add_row(name = "year", type = "quantitative", level = "interval", dc = "discrete") %>%
  add_row(name = "cyl", type = "quantitative", level = "ratio", dc = "discrete") %>%
  add_row(name = "trans", type = "qualitative",level = "nominal", dc = "discrete") %>%
  add_row(name = "drv", type = "qualitative", level = "nominal", dc = "discrete") %>%
  add_row(name = "cty", type = "quantitative", level = "ratio", dc = "continous") %>%
  add_row(name = "hwy", type = "quantitative", level = "ratio", dc = "continous") %>%
  add_row(name = "fl", type = "qualitative", level = "nominal", dc = "discrete") %>%
  add_row(name = "class", type = "qualitative", level = "nominal", dc = "discrete")
str_mpg1
##7d
head(str_mpg1)
#7e
str_mpg1 %>% subset(type == "quantitative" & dc == "discrete") 



################









































