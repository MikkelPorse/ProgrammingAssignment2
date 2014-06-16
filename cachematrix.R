# This file contains two functions
# makeCacheMatrix - which wraps a matrix making it possible to store the matrix inverse
# and
# cacheSolve - a function which calculates the inverse of the matrix wrapped using the former method
# and stores it for future use.
#
# As the project description states that input can be assumed to be invertible matrices
# all parameter checks have been removed.


# makeCacheMatrix creates a wrapper around a matrix x
# It exposes get/set methods to access the wrapped matrix
# and getInverse/setInverse to access the (possibly unset) inverse
#
# parameter x a matrix to wrap. 
# returns a list with functions get,set,getInverse,setInverse which 
# wraps a matrix and a a possible inverse
makeCacheMatrix <- function(x = matrix()) {

  # cached inverse - intialized to NULL
  theInverse <-NULL
  
  #getter of the wrapped matrix, x
  get <- function() x
  
  # setter of wrapped matrix - clears cached inverse
  set <- function(y) {
    x <<- y
    theInverse <<- NULL
  }
  
  # getter of the cached inverse
  getInverse <- function() theInverse
  
  #set the cached inverse
  setInverse <- function(newInverse) {
    theInverse <<- newInverse
  }
  
  #return the matrix "wrapped" in a list with 4 getters/setters 
  list(get=get, set = set, getInverse= getInverse, setInverse=setInverse )
}


# cacheSolve calculates and caches the inverse of a matrix 
# wrapped as a cacheMatrix. 
#
# parameter x = a cacheMatrix to solve
# returns the inverse of the matrix wrapped by x and stores the result in x
cacheSolve <- function(x,...) {
    
  # Return cached inverse if it's there
  inverse <- x$getInverse()
  if(!is.null(inverse))
    return(inverse)
  
  # get the matrix
  data <- x$get()

  # Get the inverse and store in cacheMatrix
  inverse <- solve(data)
  x$setInverse(inverse)
    
  # return the value
  inverse
}