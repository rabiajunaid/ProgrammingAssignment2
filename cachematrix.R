makeCacheMatrix <- function() {
  set <- function(theMatrix = matrix()) {
    theCachedMatrix <<- theMatrix
    theCachedInverse <<- NULL
  }
  get <- function() theCachedMatrix
  setInverse <- function(theInverse) theCachedInverse <<- theInverse
  getInverse <- function() theCachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(matrixObject, ...) {
  inv <- matrixObject$getInverse()
  if(!is.null(inv)) {
    message("Getting cached matrix inverse.")
    return(inv)
  }
  mx <- matrixObject$get()
  inv <- solve(mx, ...)
  matrixObject$setInverse(inv)
  inv
}
