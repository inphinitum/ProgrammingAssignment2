## This file contains two functions meant to address the second programming assignment from the R programming Coursera
## course.
##
## makeCacheMatrix creates a special matrix object that stores the given matrix and provides four methods to manipulate
##  it: set, get, setInverse, getInverse. These four methods will give control over a matrix and its inverse
##
## cacheSolve is an optimized auxiliary function to calculate the inverse of the matrix contained in an object created via
##  makeCacheMatrix. If the inverse of the matrix is already available it does not calculate it again and uses the already
##  available data.


## makeCacheMatrix
## Given a square matrix creates an "object" with four methods, set and get the matrix, set and get the inverse of the
## given matrix.
##
## If the given matrix is not square the setInverse function will fail and throw an error.
##
## Returns:
##  - A list with four elements: set, get, setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  getInverse <- function() inverse
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve
##
## Computes the inverse of an matrix object created with "makeCacheMatrix". If the inverse has already been calculated
## the cachesolve returns the previously calculated inverse, unless the matrix has changed.
##
## Returns:
##  - The inverse of the given object

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("Using cached data")
    return(inverse)
  }
  
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  
  inv
}
