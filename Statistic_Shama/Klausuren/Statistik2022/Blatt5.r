library(tidyverse)
library(psych)
library(DescTools)

##Aufgbae 5a

n <-  10

toses <- tibble(no  = 1:n, res = sample (c (0,1), size = n, replace = TRUE ),
                rel.freq = cumsum(res)/no)
toses
#5b

n <-  100 # 1000

toses <- tibble(no  = 1:n, res = sample (c (0,1), size = n, replace = TRUE ),
                rel.freq = cumsum(res)/no)
toses

plot (x = toses$no, y= toses$rel.freq)
abline (a= 0.5, b= 0)










a<- sample(c("b", "r", "g"), size = 10, replace = TRUE);a







