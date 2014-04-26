## This set of functions will first create an inverse of a matris
## and cache the value for later use.  The second section will
## recall the cache when available. Otherwise it will calculate
## the inverse of the matrix and store it.


##This function creates a special "matrix" object 
##that will cache the inverse value of that matrix

makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse value to NULL
  Inv <- NULL  
  #set the matrix value
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  #get the matrix value
  get <- function() x
  #set inverse
  set_inverse <- function(inv) Inv <<- inv
  #get inverse
  get_inverse <- function() Inv
  # return the list functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)    
}

## This function checks to see if there is a cache of the inverse. 
## If so, it retrieves the inverse from the cache 
## Otherwise, it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
  # check for cached inverse
  Inv <- x$get_inverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  #compute inverse
  Inv <- solve(data, ...)
  x$set_inverse(Inv)
  Inv
}