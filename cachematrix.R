## These functions store a variable in the cache for fast access

## This function takes a matrix as an input and returns a list with functions to be accessed later.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { # This function is used to set new values for our matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x # Gets our matrix values
  setInverse <- function(inverse) inv <<- inverse # Sets the values of the inverse into a cached variable
  getInverse <- function() inv # Gets the values of our inverse matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## In this function first it is checked if there is already a cached value for the inverse. If that is the case, it returns it. 
## Otherwise, the mean is calculated and stored in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) { # Checks what I explained above
    message("getting cached data")
    return(inv)
  }
  mat <- x$get() #Calculates the inverse matrix
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
