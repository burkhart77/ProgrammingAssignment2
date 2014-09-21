## These cache matrix functions will allow you to make a 
## special cached inverse matrix and provides the ability
## to retrieve it from cache memory rather than recomputating
## the inverse each time.

## NOTE: the provided matrix must be mathmatically able to 
## be inverted, i.e. square and not singular or Lapack routine
## dgesv will return an error upon using solve().

## makeCacheMatrix is a function which creates a special 
## matrix object to store in cache the inverse of a given
## matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # Set up special matrix which can be used outside this 
  # environment.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Set up functions to work on the matrix
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}


## Returns a matrix that is the inverse of 'x'. If it 
## has already been calculated it will retrive it from 
## cache memory rather than recomputing.

cacheSolve <- function(x, ...) {
  
  ## If the inverse is stored in memory skip
  ## recomputation, retreive from memory and
  ## exit the function.
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting cached data.")
    return(m)
  }
  
  ## Inverse has not been stored, compute it, set cache
  ## matrix, and return inverse.
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
