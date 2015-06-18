## Functions that cache the inverse of a matrix.

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix". above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      t<-makeCacheMatrix(x)
      ## Return a matrix that is the inverse of 'x'
      inv <- t$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- t$get()
      inv <- solve(data, ...)
      t$setinverse(inv)
      inv
}
