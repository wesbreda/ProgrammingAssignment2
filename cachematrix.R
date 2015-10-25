##cachematrix.R
##Programming Assignment 2
## 10-25-2015

## These functions cache information about the inverse of a matrix.
## The resulting calculation can be saved to prevent multiple executions.
## If the matrix changes, the cache value is deleted.

## This section creates a list of four functions.
##1.  Sets the value of the matrix
##2.  Gets the value of the matrix
##3.  Sets the value of the inverse
##4.  Gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set_matrix <- function(new_matrix){
            x <<- new_matrix
            m <<- NULL
      }
      get_new_matrix <- function() x
      set_inverse <- function(solve) m <<- solve
      get_inverse <- function() m
      list(set_matrix = set_matrix,
           get_new_matrix = get_new_matrix,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}


## Calculates the inverse of the matrix and caches the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
      m <- x$get_inverse()
      if(!is.null(m)) {
              message("getting cached data")
              return(m)
      }
      data <- x$get_new_matrix()
      m <- Solve(data, ...)
      x$set_inverse(m)
      m

}
