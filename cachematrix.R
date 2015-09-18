## There is a set of functions to compute and cache the inverse of a matrix.
##
## How to use:
## 
## max <- matrix()
## matrix_object <- makeCacheMatrix(max)
## cacheSolve(matrix_object)
## 
## 
## This function (makeCacheMatrix) crates the list of 4 function for an matix "object":
##    get() - returns the x matrix stored in the main function.
##    set() - can be used to change x matrix
##    setcache() - saves the inversed matrix of x
##    getcache() - returns the inversed matrix of x


makeCacheMatrix <- function(x = matrix()) {
 
  cache <- NULL
  
  set <- function(y){
    x <<- y
    cache <<- NULL  }
  get <- function() x
  setcache <- function(z) cache <<- z
  getcache <- function() cache
  
  list(set = set, get = get, setcache = setcache, getcache = getcache)
  
}

## This function (cacheSolve) computes the inverse of the matrix "object" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

  cache <- x$getcache() ## getting inverse matrix
    if(!is.null(cache)) ## if exists - return it
      {
      message("getting cached data")
      return(cache)
      }
    my_matrix <- x$get()            ## else - get the matrix
    cache <- solve(my_matrix, ...)   ## compute the inverse
    x$setcache(cache)                 ## save the result
    cache                              ## and return it
}

############
## Here is a test to verify.
##
## max <- matrix(c(1:4,8:12), nrow = 3)
## matrix_object <- makeCacheMatrix(max)
## cacheSolve(matrix_object)
