## This function creates a matrix that has special attributes for storing its inverse as well
## as accessing it.
## 4 helper functions:
##    - set the matrix
##    - get the matrix
##    - set the matrix inverse
##    - get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(inverse) i <<- inverse
  getMatrixInverse <- function() i
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## This funtion checks to see if there is a cached version of the inverse matrix for the 
## incoming argumrnt and returns it if found. If not found, the inverse is calculated and
## added to the cache before returned to the caller.

cacheSolve <- function(x, ...) {
  i <- x$getMatrixInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setMatrixInverse(i)
  i
}
