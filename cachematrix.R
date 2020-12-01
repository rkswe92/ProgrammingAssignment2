## Put comments here that give an overall description of what your
## functions do

## This functions creates a list of get,set and getinverse and setinverse functions
## which cache the values of matrices inverse once calculated
## this funtion always returns a list with get,set,getinverse and setinverse function on a inputted matrix

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


## cachesolve computes the inverse of inputed list 

cacheSolve <- function(x, ...) {
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
