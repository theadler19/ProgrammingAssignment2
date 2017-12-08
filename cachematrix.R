## Calculating the inverse of matrix is computationally intensive
## As such, we should only perform this exercise in the event of distinct matrices 
## Functions below allow a matrix inverse, once calculated, to be cached for efficient operation

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     
     j <- NULL
     set <- function(y) {
          x <<- y
          j <<- NULL
     }
     
     get <- function() x
     setinverse <- function(inverse) j <<- inverse
     getinverse <- function() j
     list(set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     j <- x$getinverse()
     if (!is.null(j)) {
          message("Retreiving cached data")
          return(j)
     }
     data <- x$get()
     j <- solve(data, ...)
     x$setinverse(j)
     j
}
