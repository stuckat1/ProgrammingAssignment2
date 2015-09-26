##  makeCacheMatrix and cacheSolve are designed to work together to create matrices that 
# are capable of caching the computationally expensive operation of matrix inversion.

## Description:n
# This function returns a "special" matrix that has standard get/set accessor methods.
# In addition, when used in conjunction with cacheSolve function, the matrix inverse
# computation are automatically cached. 
makeCacheMatrix <- function(x = matrix()) {
  
  # clear inverted matrix during first call
  i <- NULL
  
  # set new matrix
  set <- function(y) {
    x <<- y      # set new matrix
    i <<- NULL   # clear inverted matrix cached value 
  }
  
  # simple get/set routines
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  
  # add methods for this object
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Description
# This function returns the inverse of the "special" matrix provided.
# All computations are cached and stored inside the matrix 
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()     # get the inverse from special matrix
  
  # if there is a meaningful value, return it and we are done 
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  # ... else we compute the inverse, store it and return it
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
  
}