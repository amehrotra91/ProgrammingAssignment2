## functions do

## Function to set and get a matrix

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  
  setMatrix <- function(newValue) {
    x <<- newValue
    # since the matrix is assigned a new value, flush the cache
    cache <<- NULL
  }
    getMatrix <- function() {
      x
    }
    
    # cache the given argument 
    cacheInverse <- function(solve) {
      cache <<- solve
    }
    
    # get the cached value
    getInverse <- function() {
      cache
    }
    
    # return a list. Each named element of the list is a function
    list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## Function to solve cache issue

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get the cached value
  inverse <- x$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- x$getMatrix()
  inverse <- solve(data)
  x$cacheInverse(inverse)
  
  # return the inverse
  inverse
}
