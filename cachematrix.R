## Put comments here that give an overall description of what your
## functions do
##The following functions are used to create a special object that stores a matrix and caches its inverse. The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
##1)set the value of the matrix
##2)get the value of the matrix
##3)set the value of the inverse
##4)get the value of the inverse
## A pair of functions that cache the inverse of a matrix
## Creates a special matrix object that can cache its inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse property
        i <- NULL
        ## Method to set the matrix
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
         ## Return a list of the methods
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
##This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
