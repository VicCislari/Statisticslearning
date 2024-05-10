
#######################################################
# examples: gather(), spread() and separate()
# file: ex_gather_spread_separate.R
#######################################################

# working directory
setwd("C:/Users/Egbert Falkenberg/Nextcloud/NextCloud Lehre/aktuelle_LV/Statistik/Slides/examples_R/Intro-R")

# tidyverse einbinden
library(tidyverse)

#######################################################
# create messy data
#######################################################
# load data
tidyr::who

# creating tidy data from who 
whocleaned <- who %>%
  gather(key = "key", value = "cases", new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  mutate(key = stringr::str_replace(key,"newrel","new_rel")) %>%
  separate(key,c("new", "type", "sexage"), sep = "_") %>%
  separate(sexage, c("sex", "age"), sep = 1) %>% 
  select(-new, -iso2, -iso3)
whocleaned
# create a table containing country and for every the 
# population and the number of all infections
population 
table1 <- inner_join(
  population,
  whocleaned %>% count(country, year, wt = cases),
  by = c("country", "year")
)
table1

# messy data: one observation in more than one row
# create a table containing a column type with
# values cases and population and a column count, which
# gives the corresponding number of cases resp. of 
# population
table2 <- table1
# rename the columns cases and population the "count"
names(table2)[3:4] <- c("count","count")
table2 <- bind_rows(
  # only values of population per country and year
  table2[,1:3] %>% mutate(type = "population"),
  # only values of cases per country and year
  table2[, c(1,2,4)] %>% mutate(type = "cases")
) %>% 
  # sort by country and year
  arrange(country,year)
table2

# messy data: values of two columns in one column
table3 <-
  table1 %>%
  unite(rate, n, population, sep ="/")
table3

# messy data: column names are not names of variables 
# but values of a variable
# only values of population per country in the years
# 1999, 2000 and 2001
table4a <- 
  table1[,1:3] %>% 
  filter(year >= 1999 & year <= 2001) %>%
  spread(year, population) 
table4a

# only values of cases per country in the years
# 1999, 2000 and 2001
table4b <-
  table1[,c(1,2,4)] %>% 
  filter(year >= 1999 & year <= 2001) %>%
  spread(year, n) 
table4b

# messy data speichern
setwd("C:/Users/Egbert Falkenberg/Nextcloud/NextCloud Lehre/aktuelle_LV/Statistik/Slides/examples_R/Intro-R")
table4a %>% write_csv("table4a.csv")
table3 %>% write_csv("table3.csv")
table2 %>% write_csv("table2.csv")

#######################################################
# example gathering
#######################################################
# messy data: column names are not names of variables 
# but values of a variable
table4a <- read_csv("table4a.csv")
head(table4a)
table4a
# tidied data
table4a %>%
  gather('1999', '2000', '2001', 
         key = "year",
         value = "cases") %>%
  # sort by country, year
  arrange(country, year) %>%
  head
# applying the new function pivot_longer()
table4a %>% 
  pivot_longer(cols = c('1999','2000','2001'), names_to = "year",
               values_to = "cases")

#######################################################
# example spreading
#######################################################
# messy data: one observation in more than one row
table2 <- read_csv("table2.csv")
head(table2)
# tidied data
table2 %>%
  spread(key = type, value = count) %>% 
  head
# applying the new function pivot_wider()
table2 %>% 
  pivot_wider(names_from = type, values_from = count)

#######################################################
# example separating
#######################################################
# messy data: values of two columns in one column
table3 <- read_csv("table3.csv")
head(table3)
# tidied data
table3 %>% 
  separate(rate,into=c("cases","population"),sep ="/") %>%
  head()

