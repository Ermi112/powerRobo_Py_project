# makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize inverse as NULL
  
  # Set the matrix and reset the cached inverse
  set <- function(y) {
    x <<- y  # Set the matrix
    inv <<- NULL  # Reset the cached inverse
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions to interact with the matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve computes the inverse of the special "matrix" created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  
  # If the inverse is cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # Otherwise, compute the inverse and store it in the cache
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse using solve()
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the computed inverse
}
