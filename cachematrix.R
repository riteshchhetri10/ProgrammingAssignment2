## Put comments here that give an overall description of what your
## functions do

# Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
  cache <- NULL  # Initialize the cache for storing the inverse
  set <- function(matrix) {
    m <<- matrix  # Set the matrix
    cache <<- NULL  # Reset the cache when the matrix changes
  }
  get <- function() m  # Get the matrix
  setcache <- function(inverse) cache <<- inverse  # Set the cache with the inverse
  getcache <- function() cache  # Get the cached inverse
  
  list(set = set, get = get, setcache = setcache, getcache = getcache)
}

# Compute the inverse of the special "matrix" and cache it
cacheSolve <- function(x, ...) {
  cache <- x$getcache()  # Check if the inverse is already cached
  if (!is.null(cache)) {
    message("Using cached data")  # Return the cached inverse if available
    return(cache)
  } else {
    data <- x$get()  # Get the matrix
    inv <- solve(data, ...)  # Calculate the inverse
    x$setcache(inv)  # Cache the inverse for future use
    return(inv)
  }
}

