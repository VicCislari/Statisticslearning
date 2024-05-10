# Sheet 3: Case Study - Tidy Data'
# WS 2019/20

# load libraries and the data set who
library(tidyverse)
# pkg::name returns the value of the exported variable name
# in namespace pkg
tidyr::who 

# Clean the data set in several steps

# a) Identify columns that are not variables.
# Inspect the columns 
?tidyr::who
# A subset of data from the World Health Organization Global

# Which are columns are variables?
# country, iso2, and iso3 are three variables that redundantly 
# specify the country.
# year is clearly also a variable.
# From the structure in the variable names (e.g. new_sp_m014,
# new_ep_m014, new_ep_f014, ...) these arelikely to be values, 
# not variables.

# b) Inspect the gather() command and apply the command to gather
# together all the columns from new_sp_m014 to newrel_f65. Since 
# we do not know what the values represent, give them the 
# generic name ``key''. The cells represent the count of cases, 
# therefore use the variable cases. Remove the missing values in 
# the  current representation using na.rm.

# The gather() command
?gather()

# apply gather()
who1 <- who %>%
  gather(key = "key", value = "cases",
         new_sp_m014:newrel_f65,
         na.rm = TRUE)
who1

# c) Count the values in the new ``key'' column.
key.values <- who1 %>% count(key)
key.values
# d) The values of the new column ``key'' have the following 
# structure:

# The first three letters of each column denote whether the 
# column contains new or old cases of TB. In this dataset, 
# each column contains new cases. 

# The next two letters describe the type of TB:
# rel stands for cases of relapse
# ep stands for cases of extrapulmonary TB
# sn stands for cases of pulmonary TB that could not be diagnosed
# by a pulmonary smear (smear negative) 
# sp stands for cases of pulmonary TB that could be diagnosed be 
# a pulmonary smear (smear positive) 

# The sixth letter gives the sex of TB patients. The dataset 
# groups cases by males (m) and females (f). 

# The remaining numbers gives the age group. The dataset groups 
# cases into seven age groups: 
# 014 = 0 - 14 years old
# 1524 = 15 - 24 years old 
# 2534 = 25 - 34 years old 
# 3544 = 35 - 44 years old 
# 4554 = 45 - 54 years old
# 5564 = 55 - 64 years old 
# 65 = 65 or older

# Unfortunately the names are slightly inconsistent because 
# instead of  new_rel we have newrel. Use str_replace() command 
# to replace the characters ``newrel`` with ``new_rel``.
who2 <- who1 %>%
  mutate(key = 
           stringr::str_replace(key,"newrel","new_rel"))
who2$key %>% unique

# Split the codes at each underscore
who3 <- who2 %>%
  separate(key,c("new", "type", "sexage"), by = "_")
who3
```
# Separate the values of sexage after the first character
who4 <- who3 %>%
  separate(sexage, c("sex", "age"), sep = 1)
who4

# e) Remove the redundant columns new, iso2 and iso3.
who5 <- who4 %>% select(-new, -iso2, -iso3)
who5


# 3) Clean the data set using pipes
whocleaned <- who %>%
  gather(key = "key", 
         value = "cases", 
         new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  mutate(key = 
           stringr::str_replace(key,"newrel","new_rel")) %>%
  separate(key,c("new", "type", "sexage"), by = "_") %>%
  separate(sexage, c("sex", "age"), sep = 1) %>% 
  select(-new, -iso2, -iso3)
whocleaned

# 4) Create a table containing country and for every the 
# population and the number of all infections. Use the function 
# tally/count().
population # Subset of data from the WHO Global Tuberculosis Report
table1 <- right_join(
  population,
  whocleaned %>% count(country, year, wt = cases),
  by = c("country", "year")
)
table1
