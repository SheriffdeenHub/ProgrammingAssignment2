
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # cache for the inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # clear cached inverse when matrix changes
  }
  
  get <- function() x  # return the matrix
  
  setinverse <- function(inverse) inv <<- inverse  # cache the inverse
  getinverse <- function() inv  # retrieve cached inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)  # return cached inverse
  }
  
  data <- x$get()
  inv <- solve(data, ...)  # compute the inverse
  x$setinverse(inv)  # cache the result
  inv
}
