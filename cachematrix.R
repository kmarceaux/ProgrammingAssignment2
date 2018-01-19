## Assignment is to cache the inverse of a matrix.  Below are the pair of functions
## that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        INV = NULL
        set = function(y) {
                x <<- y
                INV <<- NULL
        }
        get = function() x
        setINV = function(inverse) INV <<- inverse
        getINV = function()  INV
        list(set=set, get=get, setINV=setINV, getINV=getINV)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        INV = x$getINV()
        if(!is.null(INV)) {
                message("getting cached data")
                return(INV)
        }
        data <- x$get()
        INV <- solve(data, ...)
        x$setINV(INV)
        return(INV)
}
