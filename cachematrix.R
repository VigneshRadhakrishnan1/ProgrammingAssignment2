## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly
##
## The code assumes that the matrix entered can be inversed. Exception handling not covered
##
## Inverse of a matrix can be calculated using the solve function in R

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## the function does the following
## 1. set the value of the matrix 
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL


## 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL                      }
  
## 2. get the value of the matrix
  get <- function() x
  
## 3. set the value of the inverse of the matrix
  solve_set <- function(solve) m <<- solve
  
## 4. get the value of the inverse of the matrix
  solve_get <- function() m
  
## return the list of values
  list(set = set, get = get,solve_set = solve_set,solve_get = solve_get)
}

## cacheSolve function will return the inverse , given a Matrix object- 
## from the cache if it already exists, else it will calculate it. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$solve_get() # retrieve from cache
  
  if(!is.null(m)) { # some value present in the cache
    message("getting cached data")
    return(m)
  }
  
  # else if nothing in cache
  data <- x$get()       # get the matrix
  m <- solve(data, ...) # solve the inverse
  x$solve_set(m)	      # set the cache
  m		                  # return 
}