# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {  # Default argument is an empty matrix
  
  inv <- NULL  # Initialize 'inv' (which will store the cached inverse) as NULL
  
  # This function sets the matrix 'x' and clears the cached inverse 'inv'
  set <- function(y) {
    x <<- y  # Assigns the value of 'y' to 'x' in the parent environment
    inv <<- NULL  # Clears the cached inverse
  }
  
  # This function returns the matrix 'x'
  get <- function() x
  
  # This function sets the cached inverse 'inv'
  setinverse <- function(inverse) inv <<- inverse
  
  # This function returns the cached inverse 'inv'
  getinverse <- function() inv
  
  # The function returns a list of the four functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of a special "matrix" object created by the 'makeCacheMatrix' function.
cacheinverse <- function(x, ...) {  # 'x' is expected to be a 'makeCacheMatrix' object
  
  inv <- x$getinverse()  # Try to get the inverse from the cache
  
  # If the cached inverse is not NULL, it has been computed before
  if(!is.null(inv)) {
    message("getting cached data")  # Print a message indicating that the cached inverse is being used
    return(inv)  # Return the cached inverse
  }
  
  # If the cached inverse is NULL, it has not been computed before
  matrix_to_invert <- x$get()  # Get the matrix from the 'makeCacheMatrix' object
  inv <- solve(matrix_to_invert, ...)  # Compute the inverse of the matrix
  
  x$setinverse(inv)  # Store the computed inverse in the cache
  
  inv  # Return the computed inverse
}
