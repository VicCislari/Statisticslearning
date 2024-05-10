library (tidyverse)

df <- tibble ( id = 1 : 10 ,
                     sex = sample( x =c ( " f " , "m" ) , size = 10 ,
                                   replace = TRUE) ,
                     age = round( runif ( 10 , 20 , 35 ) ) ,
                     score1 = round( runif ( 10 , 0, 25 ) )
)

#alle daten von mann
df %>%
  filter( sex == "m")

#daten von eine neue person fügen (id , alter...)

df<-
  add_row(df, id  = 11, sex = "m",age = 25, score1= 4)
df

# 2 spalten score 2 und score 3 einfügen with random nr. zwishcen 0 und 25
#die scores werden zusammen gerechnet und in grade (noten) verteilt

df<-
  df%>%
  mutate(score2 = round(runif(11,0,25)))%>%
  mutate(score3 = round(runif(11,0,25)))%>%
  mutate (scoresum  = score1+score2+score3)%>% ##score 1 2 und 3 werden addiert

  mutate(grade = case_when(
    scoresum <= 37 ~ 5,
    scoresum > 37 & scoresum <= 45 ~4,
    scoresum > 45 & scoresum <= 55 ~3,
    scoresum > 55 & scoresum <= 65 ~2,
    scoresum > 65 ~1))

df















