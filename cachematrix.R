## Author : Julie Koesmarno (http://www.mssqlgirl.com)
## Version: 1.0 - 23 May 2015

##
## makeCacheMatrix creates a special "matrix" object that allows
## caching its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
  ## Initialise i (to hold inverse value)
  i <- NULL
  
  ## "set" method initialises 
  ##  * x with y
  ##  * i with NULL (reset)
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## "get" method returns the value of x
  get <- function() x
  
  ## "setInverse" method stores the inverse value
  setInverse <- function(inverse) i <<- inverse
  
  ## "getInverse" method returns the inverse value
  getInverse <- function() i
  
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##
## cacheSolve calculates the inverse of the special "matrix" object returned
## by makeCacheMatrix above. Previously calculated inverse for the same matrix
## value will be retrieved from cache (and will not be recalculated)
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Get the inverse value in the cache (makeCacheMatrix)
  i <- x$getInverse()
  
  ## Return the cached inverse, if not NULL
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Get the matrix value, calculate the inverse, store
  ## it back in the cache and return the inverse.
  data <- x$get()
  i <- solve(data)
  x$setInverse (i)
  i
}
