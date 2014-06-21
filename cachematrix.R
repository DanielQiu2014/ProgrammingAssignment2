# Matrix inversion is usually a costly computation 
# and their may be some benefit to caching the inverse 
# of a matrix rather than compute it repeatedly 
# the following are a pair of functions that cache the 
# inverse of a matrix.

# makeCacheMatrix function creates a special "matrix" 
# object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  I <- NULL              #set inverse matrix to NULL
  set <- function(y) {
    x <<- y              #store matrix to x
    I <<- NULL           #set inverse matrix to NULL
  }
  get <- function() x    # return stored matrix
  setinv <- function(inverse) I <<- inverse #set inverse matrix to I
  getinv <- function() I # return stored inverse matrix
  
  # put the four functions to a list which is returned
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
  
  I <- x$getinv() #set inverse matrix to I
  
  # check if I contain any inverse matrix
  if(!is.null(I)) {   
    
    # if I contain an inverse matrix..
    message("getting cached data")
    return(I)
  }
  
  # if I does not contain an inverse matrix..
  data <- x$get() # get matrix
  I <- solve(data, ...) # calculate inverse matrix
  x$setinv(I) # coach inverse matrix
  I           # return inverse matrix
}
