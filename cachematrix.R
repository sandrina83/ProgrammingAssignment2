## Put comments here that give an overall description of what your
## functions do

#This script contains two functions.
#The first one creates a matrix and caches its reverse
#The second one returns a matrix that is the inverse of 'x'

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    #set the value of the matrix
    x <<- y
    m <<- NULL
  }
  #get value of the matrix
  get <- function() x 
  
  #set Inverse
  setInverse <- function(solve) m <<- solve 
  
  #get inverse
  getInverse <- function() m 
  
  #output (a list)
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#Function that returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  #check if Inverse has already been calculated
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #compute Inverse
  data <- x$get()
  m <- solve(data, ...)
  
  #set Inverse (cache)
  x$setInverse(m)
  m
}
