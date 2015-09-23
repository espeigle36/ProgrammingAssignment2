## The following functions, makeCacheMatrix and cacheSolve, allow
## the user to cache the inverse of the matrix and return when needed

## makeCacheMatrix creates a function with a list of functions -
## set, get (store and return original matrix),
## setinverse, and getinverse (store and return inverted matrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve takes the matrix from makeCacheMatrix and computes the
## inverse if not already calculated, otherwise returns inverse from cache

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
