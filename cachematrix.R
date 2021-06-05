## Writing a pair of functions to cache the inverse of a matrix

## x is a square invertible matrix. This function will return a list containing
## functions to set the matrix, get the matrix, set the inverse of the matrix
## using the solve function and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv = function(inverse) m <<- solve(x)
  getinv = function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The following function calculates the inverse of the matrix created with the 
## above function. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets it from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  if (!is.null(m)){ 
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setinv(m)
  
  return(m)
}


## Checking if the above functions work
A <- matrix(c(1,2,3,4),2,2)

AInv <- makeCacheMatrix(A)
cacheSolve(AInv)

cacheSolve(AInv)