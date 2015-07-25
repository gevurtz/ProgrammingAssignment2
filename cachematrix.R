## These two functions cache the inverse of a square, invertible matrix.

## Function makeCacheMatrix creates a matrix object that can cache its inverse
## using cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x 
      setinv <- function(inv) i <<- inv
      getinv <- function() i
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
      
}

## Function cacheSolve computes and returns the inverse of the matrix created
## by makeCacheMatrix. If the inverse has already been calculated then
## cacheSolve returns the cached inverse.
cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}