## This pair of functions caches the inverse of a matrix.

## This function creates a "matrix" object that returns a matrix that sets and
## gets the matrix, and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function (y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache and indicates that cached dat is being returned.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
         message("getting cached data")
         return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
}
