## Together, these two functions work to enable the caching of calculations of inverse matrices. The 1st function is created
## to be used by the 2nd function to determine if the inverse to the supplied matrix has been calculated and cached before. 

## Note, these functions only work for square matrices, Can use ginv() from the MASS project if looking to solve non-square matrices.


## Function 1
## Returns a special object(list) of type "makeMatrix()" which can be used in parent environments for other functions.
makeMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Function 2
## Function that checks cache (utilising logic from Function 1) to see if a solution has already been found for calculating the inverse of matrix x..
## If found, retrieves it, if not found, calculates inverse of supplied matrix x.

## Note, matrices must be created via the "makeMatrix()" so they are created as a list. A standard matrix is of type "matrix" so
## will fail if you attempt to run "cacheInverse()" on it.

cacheInverse <- function(x, ...) {
  
  ## Checks cache to identify if inverse matrix already exists, retrieves if exists and returns it
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Retrieving cached data.")
    return(m)
  }
  ## If target matrix not found in cache (therefore identity matrix doesn't exist) - calculates inverse matrix and returns it
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  message("No cached solution exists. Solving for inverse matrix now and caching result.")
  m
}