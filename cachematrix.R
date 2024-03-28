library(matlib)
## Functions creating an object wrapping a matrix and providing 
## access to the inverse, using cache

## wrap the matrix provided as argument in an object that provides
## access to the inverse, using superassignment for caching the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    }


## retrieve the cached inverse of the matrix wrapped in an 
## object constructed by makeCacheMatrix, compute and cache if
## not done previously

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(is.null(m)) {
      message("caching inverse of wrapped matrix")
      data <- x$get()
      m <- inv(data, ...)
      x$setinv(m)
    } else {
      message("using cached inverse of wrapped matrix")
    }
    m  
}
