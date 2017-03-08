makeCacheMatrix <- function(x = matrix()) {## define the argument with default mode of "matrix"
  inv <- NULL                              ## initialize inv as NULL; will hold value of matrix inverse 
  set <- function(y) {                     ## define the set function to assign new 
    x <<- y                                ## value of matrix in parent environment
    inv <<- NULL                           ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x                      ## define the get fucntion - returns value of the matrix argument
  setInverse <- function(inverse) inv <<- inverse ## assigns value of inv in parent environment
  getInverse <- function() inv            ## gets the value of inv where called
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}