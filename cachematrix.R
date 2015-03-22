## This file contains two functions.
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve returns a matrix that is the inverse of 'x'

## makeCacheMatrix is a function of x that uses a matrix as an input, and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##this is creating global variables where data can be cached and accessed later if avaliable
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## cacheSolve returns a matrix that is the inverse of 'x'
## it first looks to see if there is a cached value for matrix (m) and runs solve() to inverse it if so
## if there is not a cached value of m, it will look for a local version and run solve() on it to generate the inverse if needed

cacheSolve <- function(x, ...) {
    m <- x$get()
    if(!is.null(m)) {
      message("getting cached data")
      return(solve(m))
    }
    ##this "if" is looking to see if there is a global value of m avaliable.  if so, it will print the words "getting cached data" and use it with the solve() function to return an inverse
    data <- x$get()
    m <- solve(data)
    x$setsolve(m)
    m
    ## if not, it is taking a local copy of m and using solve to print the inverse
  }
