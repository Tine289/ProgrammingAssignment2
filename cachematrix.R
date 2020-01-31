## Programming Assignment 2
## 2020-01-31

## A pair of functions that cache the inverse of a matrix
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL                                # Initializes the s varialbe to NULL
    set <- function(y){                      # Defines the values of functions 'set', 'get', 'setsolve', and 'getsolve'.
      x <<- y                 
      s <<- NULL
    }
    get <- function() x                      
    setsolve <- function(solve) s <<- solve  
    getsolve <- function() s                 
    list(set = set, get = get,
         setsolve = setsolve,                # List of arguments used to cache its inverse matrix. 
         getsolve = getsolve)
}



## The below function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

##The objects'x and s in cacheSolve differ from those in makeCacheMatrix as they are retrieved from different environments,
##i.e the values are fetched in the former and updated in the later via getsolve and setsolve.

## The x in cacheSolve will have the value that cacheSolve is called with, i.e. makeCacheMatrix.object <- makeCacheMatrix (x)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
  s <- x$getsolve()                                    
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()                           # Fetches the current value of the x object within makeVector.object
  s <- solve(data, ...)                     # Defines and calculates the inverse matrix 'x' (makeCacheMatrix) based on the values within 'data'.
  x$setsolve(s)                             # on the values within 'data'.
  s
}

