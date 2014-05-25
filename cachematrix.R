#
# This function calculates the inverse of the matrix. 
# It first checks to see if the inverse of the matrix  has already 
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


# CacheSolve function Returns cached matrix inverse using previously computed matrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()  # get the inverse of the matrix from cache
  # check the value of m is NULL or not
  # If the inverse of the matrix is not null, get it from the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # If the inverse of the matrix from the cache  is null, then generate the 
  # inverse of the matrix 
  #
  data <- x$getinv()  # returns the original matrix
  m <- solve(data, ...)  # creates the inverse of the matrix
  x$setinv(m)  # save it in the cache
  m #It displays the inverse of the matrix
}
