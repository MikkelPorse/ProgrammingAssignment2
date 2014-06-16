# This file contains two functions
# makeCacheMatrix - which wraps a matrix making it possible to store the matrix inverse
# and
# cacheSolve - a function which calculates the inverse of the matrix wrapped using the former method
# and stores it for future use.
# both functions take only square matrices of numeric/integer types. 


# makeCacheMatrix creates a wrapper around a matrix x
# It exposes get/set methods to access the internal matrix
# and getInverse/setInverse to access the (possibly unset) inverse
#
# parameter x a matrix to wrap. Must be numeric or integer valued (or it wouldn't make much sense)
# returns a list with functions get,set,getInverse,setInverse which wraps a matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {

  # checks that input is a matrix of integers or numerics
  checkIsMatrix <- function(m){
    is.matrix(m) & ((length(m)==1 & is.na(m[1])) | class(m[0]) %in% c("integer","numeric"))
  }
  
  if(!checkIsMatrix(x))
    stop("supplied argument x is not an integer or numeric matrix")
  
  theInverse <-NULL
  get <- function() x
  set <- function(y) {
    if(!checkIsMatrix(y))
        stop("input is not an integer or numeric matrix")
    x <<- y
    theInverse <<- NULL
  }
  
  getInverse <- function() theInverse
  setInverse <- function(newInverse) {
    #inverse must also be a square integer/numeric matrix - or NULL
    if(!is.null(newInverse) & !checkIsMatrix(newInverse))
      stop("inverse matrix must be null or a square integer/numeric matrix")
    theInverse <<- newInverse
  }
  
  #return the matrix "wrapped" in a list with 4 getters/setters 
  list(get=get, set = set, getInverse= getInverse, setInverse=setInverse )
}


# cacheSolve calculates and caches the inverse of a square matrix 
# wrapped as a cacheMatrix. It will fail if matrix is not square, contains 
# NA/NAN/NULL values or is singular
# 
# parameter x = a cacheMatrix to solve
# returns the inverse of the input x and stores the result in x
cacheSolve <- function(x,...) {
  # function that inspects input x
  checkIsCacheMatrix <- function(m){
    class(m)=="list" & 
      setequal(c("get","set", "getInverse", "setInverse"), names(m))
  }
  
  # Check that x is a cacheMatrix
  if(!checkIsCacheMatrix(x))
      stop("argument must be a cacheMatrix.")
    
  # Return cached inverse if it's there
  inverse <- x$getInverse()
  if(!is.null(inverse))
    return(inverse)
  
  # get the matrix
  data <- x$get()
  # check if matrix is invertible
  if(dim(data)[1]!=dim(data)[2]){
    stop("Matrix is not invertible (not a square)")
  }
    
  if(sum(is.na(data) | is.nan(data) | is.null(data))>0){
    stop("Matrix is not invertible (contains NA/NaN/NULL")
  }
  
  if(length(data)>1 & det(data)==0 ){
    stop("Matrix is not invertible (singular)")
  }

  # Get the inverse and store in cacheMatrix
  inverse <- solve(data)
  x$setInverse(inverse)
    
  # return the value
  inverse
}
