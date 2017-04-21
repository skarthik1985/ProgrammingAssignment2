## The functions would create a matrix and store the inverse of the matrix in cache so that the cache object
## is returned from cache whenever the inverse of the same object needs to be calculated.

## The mackeCacheMatrix function takes a matrix as an input, that needs to be inversed. 
## It returns a list of functions that set and get the matrix, set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invmatrix <<- inverse
  getInverse <- function() invmatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of the matrix created by the makeCacheMatrix function.
## Before calculating, it chekcs whether the inverse of the matrix is already available in the cache. 
## If so it returns the cached object, if not it calculates the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getInverse()
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  data <- x$get()
  invmatrix <- solve(data, ...)
  x$setInverse(invmatrix)
  invmatrix
}
