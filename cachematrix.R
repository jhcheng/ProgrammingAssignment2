##===========================================================##
## Usage:
## > v <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
## > v$getinv()
## NULL
## > inv <- cacheSolve(v)
## > v$getinv() ## will return a matrix instead of NULL
## if run cacheSolve(v) second time, program will print "getting cached data", 
## and return the result from cache.
##===========================================================##

##' This function creates a special "matrix" object that can cache its inverse.
##' This object has 4 operations: get(), set(y=matrix()), getinv() and setinv(i).
##' @param a n by n square matrix, default is an empty matrix
##' @return a special matrix object can cache its inverse
##' @usage v <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
##' @author Horace Cheng
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

##' This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##' If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##' should retrieve the inverse from the cache.
##' @param special "matrix" returned by makeCacheMatrix
##' @param ... any parameter pass to solve() function
##' @return a matrix which is the inverse of 'x'
##' @usage inv <- cacheSolve(v)
##' @author Horace Cheng
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
