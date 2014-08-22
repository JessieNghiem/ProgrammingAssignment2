## Create a mattrix that is enable to cache its inverse and reduce calculation cost

## create a special mattrix whose inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(matrix) inverseMatrix <<- matrix
  getMatrixInverse <- function() inverseMatrix
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)

}


## if the inverse of mattrix is cached, return the cached one. 
## Otherwise, calculate and return the new inverse of mattrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix  <- x$getMatrixInverse() 
  
  #check whether inverse has already been calculated
  
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix  <- solve(data, ...)
  x$setMatrixInverse(inverseMatrix)
  inverseMatrix
}
