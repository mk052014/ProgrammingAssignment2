#
# This function calculates the inverse of the matrix. 
# It first checks to see if the inverse  has already 
# been calculated. If so, it gets the inverse of the matrix 
# from the cache and skips the computation. Otherwise, 
# it calculates the inverse of the matrix and sets this value  
# in the cache via the setmat function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # initializing the matrix m to NULL
  setmat <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmat <- function() x # printing the original matrix x
  setinv <- function(solve) m <<- solve # Now m will have the inverse of the matrix x
  getinv <- function() m  # Returns the matrix inverse
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}

