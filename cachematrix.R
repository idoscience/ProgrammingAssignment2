## This function has four options
## set the value of matrix
## get the value of matrix
## set the inverse of matrix
## get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
        x <<- y
        inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(invmat) inverse <<- invmat
        getinverse <- function() inverse
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## This function compute the inverse of matrix
## If the inverse is cached, the cached version is returned
## Else the inverse is computed and returned.
## The result is also cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
