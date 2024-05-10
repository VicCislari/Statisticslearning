###############################################
# Sheet II: Analysis of nycflights
#
# file: nycflights_evaluations.R
###############################################

# load libraries
library(tidyverse)
library(nycflights13)

# a)
?flights()

# b) Find all fights with more than 2 hours arrival delay.
res.b <- 
  flights %>% filter(arr_delay > 120)
res.b

# c) Find all fights with more than 2 hours arrival delay and no 
#    departure delay.
res.c <- 
  flights %>% filter(arr_delay > 120 & dep_delay <= 0)
res.c

# d) Find all fights from United, American and Delta with no arrival
res.d <- 
  flights %>% 
  filter(carrier %in% c("AA","DL","UA") & arr_delay <= 0)
res.d

# e) Find all fights from United, American and Delta in the month
#    May with more than 5 hours arrival delay sorted by carrier and
#    flight number.
res.e <- 
  flights %>%
  filter(carrier %in% c("UA","AA","DL") & month == 5 & arr_delay > 300) %>%
  select(carrier, flight) %>% 
  arrange(carrier,flight) %>%
  # remove multiple entries
  unique()
res.e

# f) Exchange the values of departure time and arrvial time in minute
#    after midnight. 
# Format HHMM or HMM, i.e. the last 2 numbers denote
# minute and the first or the first two are numbers denote
# the hour
# x %% y 	 modulus (x mod y) 5%%2 is 1
# x %/% y  integer division 5%/%2 is 2 
flights.new <-
  flights %>%
  mutate(dep_time = (dep_time %/% 100)*60 + dep_time %% 100, 
         arr_time = (arr_time %/% 100)*60 + arr_time %% 100) 
flights.new

# g) Add a column speed which denotes the average speed of the flight
#    and determine the carrier, flight of the top 10 values of speed.
res.g <- flights %>%
  mutate(speed = distance / air_time * 60) %>%
  select(carrier,flight,speed) %>%
  arrange(desc(speed)) %>% 
  top_n(10,speed)
res.g

# h) Find a list of carriers with a column ratio which denotes the number 
#    of flights with arr delay less than 10 minutes to the total number of 
#    flights. The list should be sorted by ratio.
res.h <- 
  flights %>% 
  # remove the NA's
  filter(!is.na(arr_delay)) %>%
  # boolean variable indicating a delay
  mutate(bool_del = if_else(arr_delay < 10,1,0)) %>%
  group_by(carrier) %>%
  # new columns: nof = number of flights,
  # ndel = number of delays, del_ratio = ratio 
  # values calculate per carrier
  mutate(nof = n(), 
         ndel = sum(bool_del), 
         del_ratio = ndel / nof) %>%
  select(carrier, nof, ndel, del_ratio) %>%
  # remove multiple entries
  unique() %>%
  arrange(desc(del_ratio))
res.h

# alternative solution with count()
flights %>% 
  # remove the NA's
  filter(!is.na(arr_delay)) %>%
  count(arr_delay < 10, carrier) %>% 
  group_by(carrier) %>%
  # new columns: nof = number of flights,
  # ndel = number of delays, del_ratio = ratio 
  # values calculate per carrier
  mutate(
    nof = sum(n),
    del_ratio = n / sum(n)) %>% 
  filter(`arr_delay < 10` == TRUE) %>%
  arrange(desc(del_ratio)) %>%
  select(-1)

# i) Find a list which denotes for every month the carrier with highest ratio. 
#    The list should have the columns month, carrier, number of flights of the 
#    carrier in that month and ratio.
res.i <-
  flights %>% 
  # remove NA's
  filter(!is.na(arr_delay)) %>%
  # boolean variable indicating a delay
  mutate(bool_del = if_else(arr_delay < 10,1,0)) %>%
  # Calculation grouped by carrier and month
  group_by(month,carrier) %>%
  # new columns: nof = number of flights,
  # ndel = number of delays, del_ratio = ratio 
  # values calculate per carrier
  mutate(nof = n(), 
         ndel = sum(bool_del), 
         del_ratio = ndel / nof) %>%
  # keep only 4 columns
  select(month, carrier, nof, ndel, del_ratio) %>%
  # calculation per month
  group_by(month) %>%
  # only highest ratio
  filter(del_ratio == max(del_ratio)) %>% 
  # remove multiple entries
  unique() %>%
  arrange(month)
res.i

# j) Find a table with the number of cancelled flights 
#    (dep delay = NA), the number of flights with no dep delay 
#    (-5 <= dep delay <= 5 minutes) and the means of dep delay, 
#    arr delay per month and day.
# 3 tables are generated with values per month and day 
# which are joined by these variables
res.j <-
  full_join(
    flights %>% 
      filter(is.na(dep_delay)) %>% 
      group_by(month, day) %>%
      # number of cancelled flights
      summarise(nof_canc = n())
    ,
    flights %>%
      group_by(month,day) %>%
      # number of no departure delays
      filter(dep_delay <= 5 & dep_delay >= -5) %>%
      summarise(nof_no_delay = n())
    ,
    by = c("month","day")
    ) %>%
  full_join(
    flights %>%
      group_by(month,day) %>%
      # means
      summarise(mean_dep_del = mean(dep_delay, na.rm = TRUE), 
                mean_arr_del = mean(arr_delay, na.rm = TRUE))
    ,
    by = c("month","day")
    )
res.j


# k) Table that shows, for each carrier, the flight connection (dest, origin) that 
# occurred most frequently in 2013. The table should contain only the columns
# names of airline, destination, origin and frequency and be sorted
# by frequency in descending order. 
flights %>%
  # count the number of connections
  count(carrier,origin,dest) %>%
  # filter the connections with maximal value for every carrier
  group_by(carrier) %>%
  filter(n == max(n)) %>%
  # cancel grouping
  ungroup() %>%
  # sort by n in descending order
  arrange(desc(n)) %>%
  # get the names of the carriers
  left_join(y=airlines, by="carrier") %>%
  # get the names of the airports (origin) 
  # Probleme mit by=join_by("origin"=="faa") -> by=c("origin"="faa")
  left_join(y=airports %>% select(faa,name), by=c("origin"="faa"),
            # name the column of the name by name.origin
            suffix = c("",".origin")) %>%
  # get the names of the airports (dest) 
  # Probleme mit by=join_by("dest"=="faa") -> by=c("dest"="faa")
  left_join(y=airports %>% select(faa,name), by = c("dest"="faa"),
            # name the column of the name by name.dest
            suffix = c("",".dest")) %>%
  # rename columns
  rename(
    Airline = name,
    Airport.Origin = name.origin,
    Airport.Destination = name.dest
  ) %>%
  # cancel columns not needed
  select(-carrier, -dest, -origin, n)