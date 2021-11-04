## This script employs two functions that help to save the costly computation of
## computing the inverse of a matrix. Together, they help in retrieving the previously
## calculated inverse from the cache if found again.

## This function creates a matrix object than has the ability to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <- NULL
  }
  get <- function() {
    x
  }
  set_inv <- function(inverse) i <<- inverse
  get_inv <- function()i
  
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## Write a short comment describing this function
## The below function calculates the inverse of a matrix returned from the above function
## If it has been calculated already, the inverse is returned from cache.

cacheSolve <- function(x, ...) {
  i <- x$get_inv()
  if(!is.null(i)){
    message("Getting cached matrix")
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inv(i)
  i
}
