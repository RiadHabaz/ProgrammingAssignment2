## This function creates a special "matrix" 
## The output is the following list of functions:
## set: to set the value of the matrix x
## get: to get the value of the matrix x
## setinverse: to set the value of the inverse of the matrix x
## getinverse: to get the value of the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function calculate the inverse of the special "matrix created with
## the function makeCacheMatrix
## It check if the inverse of the special matrix is already calculated:
## If it was -> it gets the the inverse of the special matrix and returns it by 
## skiping the computation
## Else -> it computes the inverse of the special matrix, set it in cache, 
## and returns it

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}