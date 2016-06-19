## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## create ONLY a square matrix (reason `solve` only handles square matrices )
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function (x = matrix()) {
      inv_x <- NULL
      set <- function (y) {
            x <<- y
            inv_x <<- NULL
      }
      get <- function () x
      setinverse<- function (inverse) inv_x <<- inverse
      getinverse <- function () inv_x
      list (set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the inverse has already been calculated, then it should retrieve 
## the inverse from the cache.


cacheSolve <- function (x, ...) {
      ## Return matrix that is inverse of 'x'
      inv_x <- x$getinverse ()
      if (!is.null(inv_x)) {
            message ("getting cached data")
            return (inv_x)
      } else {
            inv_x <- solve (x$get())
            x$setinverse (inv_x)
            return (inv_x)
      }
}
