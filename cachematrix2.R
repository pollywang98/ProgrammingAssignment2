## Matrix inversion is usually a costly computation and their may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly. This assignment is to write a
## pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.## 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  # creates a list to house the four functions
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getmatrix()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
