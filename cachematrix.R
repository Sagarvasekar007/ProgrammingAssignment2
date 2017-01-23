# There are 2 functions in this file as makeCacheMatrix and cacheSolve. 
# Purpose of these functions is to calculate the inverse of a given matrix and cache it for further use.
# Inversed cached matrix can then be used in future.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y)
  {
    x <- y
    inverse <- NULL
  }
  get <- function() x
  setMatInv <- function(inv) inverse <<- inv
  getMatInv <- function() inverse
  list(set = set, get = get,
       setMatInv = setMatInv,
       getMatInv = getMatInv)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated, then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getMatInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatInv(m)
  m
  
}
