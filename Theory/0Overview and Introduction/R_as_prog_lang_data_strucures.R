#############################################
# examples using R as a programming language
# assigments and data structures
# file: R_as_prog_language_data_strurctures.R
#############################################

# assigment of variables
a <- 2^0.5
b <- (a>2)
# get the value und display the structure
a
str(a)
b
str(b)
# show all variables in the memory
ls()
# remove a, b
rm(a,b)


#############################################
# vectors
#############################################
a <- c(1,2,3)
b <- c(2,3,4)
str(a)
str(b)
# operations are performed point by point
a+b
a*b
a+a*b
# logical vectors
c <- c(TRUE, FALSE, TRUE, TRUE)
d <- c(FALSE, FALSE, FALSE, TRUE)
# or operation
c | d
# and operation
c & d
# negation
!c

# other methods creating vectors
# : operator
3:10
# seq function
seq(1,10,by=0.5)
seq(1,3,length=5)
# rep function
rep(1.2,5)
rep(c(1.2,3.5,pi),3)

#############################################
# matrices
#############################################
# 4x4 matrix
matrix(1:16, ncol=4, nrow=4)
# 2x8 matrix
matrix(1:16, ncol=8, nrow=2)
# 2x8 matrix, rowwise input
matrix(1:16, ncol=8, nrow=2, byrow = TRUE)
# 2x8 matrix, columnwise input
matrix(1:16, ncol=8, nrow=2, byrow = FALSE)

A <- matrix(2:5, nrow=2, ncol=2)
A
# matrix operations: pointwise
A * A
A + A*A
# matrix multiplication
A %*%  A

# array function: defining arrays of arbitray dimensions
# The values in data are taken to be those in the array with the 
# leftmost subscript moving fastest. If there are too few elements 
# in data to fill the array, then the elements in data are recycled.
array(data=1:5, dim=c(2,4)) # recycle 1:5
# 3-dimensional array
array(data=1:5, dim=c(2,4,2)) # recycle 1:5 
# matrix() recycles data, too
matrix(1:8, ncol=8, nrow=2, byrow = TRUE)

# accessing elements: vectors
x <- c(2:10)
x
x[2]
x[2:4]
x[c(1,3)]
# accessing elements: matrices
M <- matrix(x, nrow = 3 ,ncol = 3)
M
M[1,2]
M[1,4] # subscript out of bounds
M[, 2] # column 2 of A
M[1, ] # row 1 of A
# logical indexing
x[x>3]
x[x>3 & x < 8]
x[x%%2 == 1] # %% denotes the modulo operator
M[M<=4]
M[M<=4 & M*M>5]
