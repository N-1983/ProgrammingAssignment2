# makeCacheMatrix: This function creates a special "matrix" object 

MakeCacheMatrix<- function(x = matrix()) 
  {
cache <- NULL
# that can cache its inverse.
set <- function(y) 
  {
x <<- y
# Set the value of the matrix
cache <<- NULL
  }
get <- function() x
# Get the value of the matrix
setMatrix <- function(inverse) cache <<- inverse
# Set the value of inverse of the matrix
getinverse <- function() cache
# Get the value of inverse of the matrix.
#Return a list with above four functions.
list(set = set, get = get,
setMatrix = setMatrix,
getinverse = getinverse)
}

cacheSolve <- function(x) 
  {
   cache<- x$getinverse()# This fetches the cached value for the inverse.
  if(!is.null(cache)) 
    { # If the cache was not empty, we can just return it
    message("getting cached data")
    
    return(cache)
    }
  data <- x$get()
  # Get value of matrix
  cache<- inverse(data)
  # Calculate inverse
  x$setinverse(cache)
  # Cache the result
cache
# Return the inverse
}