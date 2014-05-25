## These two functions can be used to calculate and store in cache
## an inverse to a matrix.  The first function, makeCacheMatrix, will
## store a new matrix in an internal variable x and will set the inverse matrix m
## to NULL.  The second function, cacheSolve, will then return the cached 
## inverse matrix if it exists, but otherwise calculate and store a new
## one if the inverse matrix is NULL.

## makeCacheMatrix will store a list of four functions that will
## set and return the value of a matrix and its cached inverse.
## The matrix and the inverse are stored as internal variables.
## When a new matrix is entered, the function will set the cached
## inverse to null.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will check if there is a cached inverse matrix
## and will return that matrix, or it will calculate a new inverse
## matrix, store it in cache, and return it.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
