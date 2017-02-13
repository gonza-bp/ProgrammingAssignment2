##Matrix inversion is usually a costly computation so 
## there is a benefit to caching the inverse of 
## a matrix rather than computing it repeatedly.
## The following functions make it possible to calculate
## and cache the inverse of a matrix storing it into a 
## special "matrix" object


## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## It is implemented using a list with the following functions:
## set: set the elements of the matrix
## get: get the elements of the matrix
## setinverse: set the inverse of the matrix
## getinverse: get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

a<-makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(a)
cacheSolve(a)
