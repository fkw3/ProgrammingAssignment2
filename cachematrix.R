## A pair of functions that used together cache the inverse of a matrix.
# Matrix inversion is usually a costly computation and
#  there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
##

# These functions assume that the matrix supplied is always invertible.

## To Test
# Create an invertable square matrix         exapmle: x <- matrix(c(2, 4, 3, 1),nrow=2,ncol=2)
# cache the mtrix using makeCacheMatrix               c <- makeCacheMatrix(x)
# call cacheSolve on the cashe                        cacheSolve(c)
##

## The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to:
#    set the value of the matrix
#    get the value of the matrix
#    set the value of the inverse
#    get the value of the inverse
##
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
#The <<- operator can be used to assign a value to an object in an environment that is different from the current environment.               
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

## The second function, cacheSolve will:
#    First checks to see if the inverse has already been calculated. 
#     if it is, it gets the mean from the cache and skips the computation
#     if not, it calculates and caches the inverse
# returned is a matrix that is the inverse of 'x'
##
cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     return(inv)    
}
