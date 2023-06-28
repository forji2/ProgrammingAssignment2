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
