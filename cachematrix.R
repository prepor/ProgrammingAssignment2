## Functions for cache result of matrix inversion.
##
## Example:
## > x <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol=2))
## > cacheSolve(x)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(x)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Make special cachable version of matrix
makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  get <- function() x
  setsolve <- function(solved) c <<- solved
  getsolve <- function() c
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Solve matrix or get cached result if already solved
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
