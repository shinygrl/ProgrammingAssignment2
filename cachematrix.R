## Put comments here that give an overall description of what your
## Description: "makeCacheMatrix" is a function that creates a special matrix object to cache its inverse of the input

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
}

## Write a short comment describing this function
## Description: "cacheSolve" is a function that computes the inverse of the special matrix and can get the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("this is the cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Now, to test the program and see what the "makeCacheMatrix" outputs.
M <- matrix(rnorm(9),3,3)
MCache <- makeCacheMatrix(M)
cacheSolve(MCache)


