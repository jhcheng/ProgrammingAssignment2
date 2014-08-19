##===========================================================##
## Usage:
## v <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
## inv <- cacheSolve(v)
## if run cacheSolve(v) second time, program will print "getting cached data", 
## and return the result from cache.
##===========================================================##

##' This function creates a special "matrix" object that can cache its inverse.
##' @title creates a special "matrix" object that can cache its inverse
##' @param a n by n square matrix
##' 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##' This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##' If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##' should retrieve the inverse from the cache.
##' @title
##' @param special "matrix" returned by makeCacheMatrix
##' @return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
