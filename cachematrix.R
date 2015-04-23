## The functions enable the caching of the inverse of a matrix.
## This is very useful for very large matrices since computing the inverse of a matrix is time-consuming.

## This function enables the caching of the inverse of a matrix.
## If the inverse of the matrix has already been computed and stored it can be retrieved using the getInversed function.
## Changing the matrix will reset the cache.
makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setInversed <- function(solved) inversed <<- solved
  getInversed <- function() inversed
  list(set = set, get = get,
       setInversed = setInversed,
       getInversed = getInversed)
}


## This function uses the cache structure above to retrieve the inverse of a matrix.
## If the inverse of the matrix has already been computed using this function, then the cached value is accessed.
## If not it is computed using the solve function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversed <- x$getInversed()
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  data <- x$get()
  inversed <- solve(data, ...)
  x$setInversed(inversed)
  inversed
}
