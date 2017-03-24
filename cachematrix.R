## This two functions allow you to store the inverse of a matrix in cache, to
## optimize resources in case the inverse is needed repeately 

## The makeCacheMatrix function is a list of functions that store a matrix getMatrix, calculates
## it's inverse getInv, store the inverse setInv. The setMatrix function allows to input a different
## matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  SetInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(setMatrix =setMatrix , getMatrix = getMatrix,
       SetInv = SetInv, getInv = getInv)
}


## The cachesolve function takes a "makeCacheMatrix object" as input
## and returns it's inverser, if the matrix hasn't change it return the
## inverse from the cache. It it has change it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
    if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data, ...)
  x$SetInv(inv)
  return(inv)
}
