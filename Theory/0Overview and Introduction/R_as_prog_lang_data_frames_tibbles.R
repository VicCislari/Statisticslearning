#############################################
# examples using R as a programming language
# factors, data frames and tibbles
# file: R_as_prog_language_data_frames_tibbles.R
#############################################

#############################################
# factors
#############################################
sex <- factor(c("male", "female", "female", "male"))
# R assigns 1 to the level "female" and 2 to the level "male" 
# (because f comes before m, even though the first element in 
# this vector is "male"). 
# structure: mention that the values are stored as integer with 
# names (levels)
sex
str(sex)
# show the levels
levels(sex)
# show the number of levels
nlevels(sex)

# Factors represent a very efficient way to store character values, because
# each unique character value is stored only once, and the data itself is 
# stored as a vector of integers. 
# Example of an ordered factor: 
mons <- c("March","April","January","November","January", "September",
         "October","September","November","August","January","November",
         "November","February","May","August","July","December","August",
         "August","September","November","February","April")
mons
str(mons)
# converting to factors
mons <- factor(mons)
mons
str(mons)
table(mons)
# Although the months clearly have an ordering, this is not reflected in 
# the output of the table function. Additionally, comparison operators 
# are not supported for unordered factors. Creating an ordered factor 
# solves these problems:
mons[1] < mons[2]
mons <- factor(mons,levels=c("January","February","March","April","May",
                             "June","July","August","September","October",
                             "November","December"),ordered=TRUE)
str(mons)
mons[1] < mons[2]
table(mons)

# Automatic string to factor conversion introduces non-reproducibility. When 
# creating a factor from a character vector, if the levels are not given 
# explicitly the sorted unique values are used for the levels, and of course the
# result of sorting is locale-dependent. Hence, the results of subsequent 
# statistical analyses can differ with automatic string-to-factor conversion in 
# place.
# -> Current version of data.frame
# data.frame(..., row.names = NULL, check.rows = FALSE,
#           check.names = TRUE, fix.empty.names = TRUE,
#           stringsAsFactors = default.stringsAsFactors())
# with default.stringsAsFactors() = FALSE which has been TRUE previously
# but has been changed to FALSE for R 4.0.0.

#############################################
# lists
#############################################
# Create a list containing strings, numbers, vectors and a logical
# values.
list.1 <- list("Red", "Green", c(21,32,11), TRUE, 51.23, 119.1)
list.1

# The list elements can be given names and they can be accessed using these names.
# Create a list containing a vector, a matrix and a list.
list.2 <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2),
               list("green",12.3))

# Give names to the elements in the list.
names(list.2) <- c("1st Quarter", "A_Matrix", "A Inner list")
list.2
# alternative
list.3 <- list("1st Quarter" = c("Jan","Feb","Mar"), 
               "A matrix" = matrix(c(3,9,5,1,-2,8), nrow = 2),
               "A inner list" = list("green",12.3))
list.3

# Elements of the list can be accessed by the index of the element in the list. 
list.3[[2]]           # use [[]] instead of []
list.3[[2]][1,]       # list.3[2][1,] causes an error
# In case of named lists it can also be accessed using the names.
list.3$`A matrix`
list.3$`A matrix`[1,]

#############################################
# data frames and tibbles
#############################################
df.grouping <- data.frame(Group = c("A","A","B","B"), 
                          Name = c("Schmidt","Olsen","Johansson","Mayer"), 
                          Age = c(22,18,21,22),
                          Sex = c("male","female","female","male"), 
                          stringsAsFactors = TRUE)
# stringsAsFactors: logical: should character vectors be converted to factors? 
# The stringsAsFactors default has been TRUE previously but has been changed to
# FALSE for R 4.0.0. Only as short time workaround, you can revert by setting 
# options(stringsAsFactors = TRUE) which now warns about its deprecation.

df.grouping
str(df.grouping)
# subseting of a dataframe leads not always to a dataframe
str(df.grouping[,3:4])
str(df.grouping[,4])
# dataframes do a partial matching
df.grouping$Gr

# tibble
library(tidyverse)
t.grouping <- tibble(Group = c("A","A","B","B"), 
                     Name = c("Schmidt","Olsen","Johansson","Mayer"), 
                     Age = c(22,18,21,22),
                     Sex = c("male","female","female","male"))
t.grouping
str(t.grouping)
# mention that subseting leads always to tibbles
str(t.grouping[,3:4])
str(t.grouping[,4])
# mention that tibbles never do partial matching
t.grouping$Gr