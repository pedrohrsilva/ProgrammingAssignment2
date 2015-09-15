# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(x) inverse <<- x # <<- operator which can be used to assign
  getinverse <- function() inverse      # a value to an object in an environment 
  list(set = set, get = get,    # that is different from the current environment.
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse))
    {
      message("there is a cache!")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
    
}
