## Coursera: R Programming -- Lexical scoping exercise
##
## The following pair of functions allow an invertible matrix to be inverted
## and the inverse cached for future return without re-calculating the inverse.

## Creates matrix object with cacheable inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Gives inverse of cacheable matrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Test functions
## Create a makeCacheMatrix object, then run the caching inverse calculation
#  twice. The first time the value is calculated, the second time the cached
## value is reported instead.

gg <- makeCacheMatrix(matrix(runif(4 ^ 2, 5, 10), 4, 4))
cacheSolve(gg) # Should calculate and cache the result
cacheSolve(gg) # Should return a cached result

round(gg$get() %*% gg$getinverse()) # Should evaluate to an identity matrix
