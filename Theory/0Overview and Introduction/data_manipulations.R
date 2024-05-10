##############################################
# Data manipulations by dplyr functions
# Examples
#
# file: data_manipulations.R
##############################################
# load libraries
library(tidyverse)
library(nycflights13)
airlines

# display flights
flights
?flights

# filter()
flights %>% filter(day == 10, month == 5)
flights %>% filter((month == 5 | month == 6), dep_time == 9)
flights %>% filter( month %in% c(3,4,10), dep_time == 9)

# arrange()
flights %>% 
  arrange(desc(dep_time), dep_delay)
arrange(flights, desc(dep_time), dep_delay) -> sort.flights
      
# select()
flights %>% select(carrier, flight) %>% unique
flights %>% select(-(dep_delay:air_time))
?select

# mutate()
flights %>% 
  select(year:day, distance, air_time) %>%
  mutate(hours = air_time / 60,
         speed = distance / hours)

# summarize()
flights %>% 
  summarise(mean_delay = mean(dep_delay),
            med_dely = median(dep_delay))

flights %>% 
  summarise(mean_delay = mean(dep_delay, na.rm = TRUE),
            med_dely = median(dep_delay, na.rm = TRUE))

flights %>% 
  group_by(carrier, flight) %>%
  summarise(mean_delay = mean(dep_delay),
            med_dely = median(dep_delay))

# NA should be not regarded in the calculation!!!
flights %>% 
  group_by(carrier, flight) %>%
  summarise(mean_delay = mean(dep_delay, na.rm = TRUE),
            med_dely = median(dep_delay, na.rm = TRUE))
