## This script contains functions to create a matrix
## and the functions to calculate and cache the inverse of the matrix
##
## For example:
## mat <- matrix(c(1, 13, 14, 12, -1, 0, -12, 4, 3), 3)
## > cmat <- makeCacheMatrix(mat)
## > cacheSolve(cmat)
## [,1]      [,2]      [,3]
## [1,] -0.09090909 -1.090909  1.090909
## [2,]  0.51515152  5.181818 -4.848485
## [3,]  0.42424242  5.090909 -4.757576
## > cacheSolve(cmat)
## getting cached solve
## [,1]      [,2]      [,3]
## [1,] -0.09090909 -1.090909  1.090909
## [2,]  0.51515152  5.181818 -4.848485
## [3,]  0.42424242  5.090909 -4.757576
 

## Make a cacheable matrix, which can be initialized as x

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x', where 'x' is a cacheable matrix
## created above from calling makeCacheMatrix

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached solve")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
