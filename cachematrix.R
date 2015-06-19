## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## makeCacheMatrix creates a matrix object that can cache its inverse
## function will do the following operations:
##    * set a matrix object
##    * get a matrix object
##    * set the inverse of the matrix given
##    * get the inverse of the matrix given
##      (if the matrix is the same, then gets it from cache)

makeCacheMatrix <- function(x = matrix()) {
  
  ## instantiate a inverse matrix holder 
  im <- NULL
  
  ## method to set the value of the matrix
  set <- function(matrix) {
    ## instantiates the matrix for function use
    x <<- matrix
    ## sets the inverse holder to null
    im <<- NULL
  }
  
  ## method to get the value of the matrix
  get <- function() {
    ## returns the matrix
    x
  }
  
  ## method to set the inverse of the matrix
  setInverse <- function(inverse) {
    ## sets the inverse holder to the inverse of the matrix
    im <<- inverse
  }
  
  ## method to get the inverse of the matrix
  getInverse <- function() {
    ## returns the inverse matrix
    im
  }
  
  ## list the methods of the function
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
##
## cacheSolve computes the inverse of a matrix returned by makeCacheMatrix
## if the inverse has already been calculated (and there is no change to the matrix),
## then the function retrieves the inverse from cache rather than re-calculating.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse matrix if not already set
  im <- x$getInverse()
  
  ## if the holder is not null, then get the cached data
  if(!is.null(im)) {
    message("getting cached data...")
    return(im)
  }
  
  ## get the matrix provided by the main makeCachematrix function
  data <- x$get()
  
  ## calculate the inverse of the provided matrix and assign it to the holder
  im <- solve(data, ...)
  
  ## set the inverse to the inverse holder object
  x$setInverse(im)
  
  ## returns the inverse matrix
  im

}
