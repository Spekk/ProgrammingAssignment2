## With these two functions we can calculate the inverse of any matrix and also cache the result in case
## it will be needed again. Like this R doesn't have to calculate again the inverse of the given matrix
## (of course if this matrix will remain the same).

## The first function, makeCacheMatrix creates a special "matrix", which is really a list of four functions. First
## we can set the values in this matrix ($set), then we can get the given matrix ($get),
## with the setinverse the inverted matrix is cached here and finally with the getinverse we can get the result.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinverse <- function(inversion) inv <<- inversion
        getinverse <- function() inv
        list (set = set, get = get, 
              setinverse = setinverse,
              getinverse = getinverse)
}

## Here is actually where the inversion is calculated with the solve function. The result is then given to the setinverse
## function to save it into the cache, so it can be called again with the getinverse if needed (through this cacheSolve function)

cacheSolve <- function(x, ...) {
        inv <- x$getinverse ()
        if(!is.null(inv)) {
                message ("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
