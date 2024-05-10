library(tidyverse)

 a<- tibble ( GENDER = sample (x=c("f","m"),size = 6, replace = TRUE),
              kurs =  c ("DEUTSCH","MAthe","ALGEBRA",NA,NA,NA),
              name = c("Sascha","Katrin","MAX","LEA","TOBI",NA)

                             
              
              

)
a %>% table(tibble$GENDER)


seq()
x <- c(sample (x = 1:5, size = 10, replace = TRUE)) # 1 bis 5 zufallszahlen bis 10
x