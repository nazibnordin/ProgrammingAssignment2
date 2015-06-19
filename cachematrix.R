## This program performs matrix inversion by caching
## the inverse of a matrix.It requires two functions,
## makeCacheMatrix that creates a special matrix object
## that caches its inverse,and cacheSolve that computes 
## the inverse of a matrix.

## makeCacheMatric creates a special matrix object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 
  inverse_x <- NULL 
  
  set <- function(y) {
     x <<- y
     inverse_x <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inverse_x <<- inverse
  getinverse <- function() inverse_x
  list(set = set, get = get, setinverse = setinverse,
        getinverse = getinverse)
}

## cacheSolve computes the inverse of a matrix create 
## in the makeCacheMatrix function.It first checks if 
## the inverse has already been calculated. If so, it
## gets the inverse from the cache and skips the computation

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  inverse_x <- x$getinverse()
  if (!is.null(inverse_x)) {
     message("getting cached inverse matrix")
     return(inverse_x)
  }
  
  matrix_data <- x$get()
  inverse_x <- solve(matrix_data)
  x$setinverse(inverse_x)
  return(inverse_x)
}
