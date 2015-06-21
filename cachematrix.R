##  This file contains a pair of functions that cache the inverse of a matrix.

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                   ##change the input matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                    ##return the input matrix
  
                                         ##manually set the output matrix(inversed matrix)
  setinverse <- function(inverse) m <<- inverse
                                         ##return the  output matrix(inversed matrix)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {                 ##In this case, 
                                    ##the inverse has already been calculated
    message("getting cached data")
    return(m)                       ##therefore,return m to finish the function
  }
  data <- x$get()                   ##get the matrix
  m <- solve(data, ...)             ##inverse the matrix
  x$setinverse(m)                   ##store the inversed matrix
  m                                 ##return the final result
}
