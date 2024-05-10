#############################################
# examples using R as a programming language
# functions
# file: R_as_prog_language_functions.R
#############################################
ch_unit <- function(unit,dat){
  if (unit == "km") {
    for (i in 1:length(dat)) {
      dat[i] <- dat[i]*1.60934
    }
    return(dat)
  } 
  if (unit == "mile") {
    # instead of a loop use
    return(dat*0.621371)
  }
  return("error")
}
x <- c(1,2,3,4,5)
ch_unit("km",x)
ch_unit("mile",x)
ch_unit("meile",x)
# display the code
ch_unit

# For example, if you have saved the above function in
# the file ch_unit.R and saved it in the directory
# c:/myPrograms, then you would proceed as follows.

# setwd(”c:/myPrograms”)
# source(file)      
# file: a connection or a character string giving the pathname of the file or URL to read from. 
source(”ch_unit”)

