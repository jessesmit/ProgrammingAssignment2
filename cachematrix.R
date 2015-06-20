## The following functions can be used together in order to calculate inverses of 
# matrices in such a way that the inverse will only be calculated once for a given
# matrix and subsquent attempts will simply return a cached value. 
#
# Usage:
#
# m <- makeCacheMatrix(matrix(1:4,2,2))
# cacheSolve(m)

## This function generates a list of functions which enable the getting and setting
# of both a matrix and the inverse of this matrix. The matrix and its inverse are
# stored within a closure and not accessible without using the accessor functions.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() {
            x
        }
        setinverse <- function(i) {
            inverse <<- i
        }
        getinverse <- function() {
            inverse 
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Using a cacheable matrix created by the above function calculate the inverse of
# a matrix in such a way that the inverse will only ever be calculated one time for
# the same matrix and subsequent calls will instead retrieve a previously calculated
# inverse.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <-solve(data)
        x$setinverse(inverse)
        inverse 
}
