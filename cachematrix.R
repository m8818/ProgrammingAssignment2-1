## The two functions makecachematrix() and cachesolve() are designed to compute the inverse of a matrix and store it in cache for further retrieval. This avoids the redundant computation times required otherwise as value in cache can be used can be used in any further function thereby.

## makecachematrix() Function is used to cache the matrix and its inverse

makecachematrix <- function(x = matrix()) {
  + xInverse <- NULL
  + set <- function(y) {
    + x <<- y
    + xInverse <<- NULL
    + }
  + get <- function() x
  + setInverse <- function(solve) xInverse <<- solve(x)
  + getInverse <- function() xInverse
  + list(set = set, get = get,
         + setInverse = setInverse,
         + getInverse = getInverse)
  + }


## cachesolve() will return the inverse of the matrix if stored in cache, otherwise it computes and then returns

cacheSolve <- function(x, ...) {
  + xInverse <- x$getInverse()
  + if(!is.null(xInverse)) {
    + message("getting cached data")
    + return(xInverse)
    + }
  + data <- x$get()
  + xInverse <- solve(data, ...)
  + x$setInverse(xInverse)
  + xInverse
  + }
