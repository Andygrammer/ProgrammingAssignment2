## Functions created to get the inverse of a matrix using caching

## This function creates a special "matrix" as a list containing a
## function to set/get values of the matrix and set/get values of
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of the special "matrix"
## created with the makeCacheMatrix function.
## If the inverse has already gotten (and no matrix changes), then
## this function returns the inverse from the cache
## Output: return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
