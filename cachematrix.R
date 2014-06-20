# Matrix inversion is usually a costly computation 
# and their may be some benefit to caching the inverse 
# of a matrix rather than compute it repeatedly 
# the following are a pair of functions that cache the 
# inverse of a matrix.

# makeCacheMatrix function creates a special "matrix" 
# object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) I <<- inverse
  getinv <- function() I
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# cacheSolve function computes the inverse of the 
# special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  I <- x$getinv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinv(I)
  I
}
