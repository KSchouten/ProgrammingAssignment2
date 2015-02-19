## These functions provide a method to cache the computed
## inverse of a matrix so it doesn't have to be recomputed
## each time. 


## This function creates a special matrix object with four
## functions: get, set, getinverse, and setinverse.
## Note that this is similar to the meanvector example from
## the assignment.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## This function is used to change the underlying matrix
  ## in this object.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## This function is used to retrieve the underlying matrix
  ## in this object,
  get <- function() x
  ## This function is used to store the computed inverse
  setinverse <- function(inverse) i <<- inverse
  ## This function is used to retrieve the stored inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the special 
## matrix object created by makeCacheMatrix.
## At the first run of this method, it will compute
## the inverse and store it in the cache (setinverse).
## Subsequent runs of this method will quickly retrieve
## the inverse from the cache and return it (getinverse).
## Again, this function is very similar to the meanvector
## example.

cacheSolve <- function(x, ...) {
  ## Check cache.
  i <- x$getinverse()
  ## If in cache, return it.
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  ## Else, compute the inverse,
  data <- x$get()
  i <- solve(data, ...)
  ## store it in cache,
  x$setinverse(i)
  ## and return it.
  i
}
