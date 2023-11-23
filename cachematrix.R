# Function to create a special matrix object with caching
makeCacheMatrix <- function() {
  # Initialize a matrix
  mat <- NULL
  
  # Function to set the matrix
  set <- function(matrix) {
    mat <<- matrix
    # Invalidate the cache when a new matrix is set
    cache$inverse <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    mat
  }
  
  # Function to compute and cache the inverse of the matrix
  cache <- list(
    inverse = NULL,
    getInverse = function() {
      if (!is.null(cache$inverse)) {
        message("Getting cached data")
        return(cache$inverse)
      }
      message("Calculating inverse and caching")
      cache$inverse <- solve(mat)
      cache$inverse
    }
  )
  
  # Return a list of functions
  list(set = set, get = get, getInverse = cache$getInverse)
}

# Function to compute the inverse of a matrix with caching
cacheSolve <- function(cacheMatrix) {
  cacheMatrix$getInverse()
}

# Example usage:

# Create a cache matrix
myMatrix <- makeCacheMatrix()

# Set a matrix
myMatrix$set(matrix(c(4, 2, 2, 1), nrow = 2))

# Get the matrix
print("Original Matrix:")
print(myMatrix$get())

# Get the inverse (computed and cached)
print("Inverse Matrix:")
print(cacheSolve(myMatrix))
