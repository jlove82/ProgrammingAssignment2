
## functions that cache the inverse of a matrix.
## Matrix inversion is usually a costly computation
## caching the inverse of a matrix have more benefit than computing it repeatedly. 

makeCacheMatrix <- function(x = matrix()) {

  invValue <- NULL
  
  set <- function(y) {
    x <<- y
    invValue <<- NULL
  }
  
  get <- function() x
  
  setInvValue <- function(inverse) invValue <<- inverse 
  
  getInvValue <- function() invValue
  
  list(set = set,
       get = get,
       setInvValue = setInvValue,
       getInvValue = getInvValue
  )
}


## function that compute the inverse of the special matrix created by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed) 
## Then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invValue <- x$getInvValue()
  if(!is.null(invValue)) {
    message("getting cached data")
    return(invValue)
  }
  
  mat <- x$get()
  invValue <- solve(mat, ...)
  x$setInvValue(invValue)
  invValue
}
