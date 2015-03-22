## This set of functions takes the invert of a matrix and stores 
## it in a cache, where it can be retrieved for further use, 
## thereby saving processing time if it's going to be used often.

## The makeCacheMatrix function creates the matrix object that
## can be used to cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) i <<- solve
        getinvert <- function() i
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)        
}


## The cacheSolve function first checks to see if the inverse of 
## the matrix returned by makeCacheMatrix has already been 
## calculated (and the matrix has not changed). If it has, 
## cacheSolve retrieves it. If it has not, it computes 
## the inverse of the special matrix that makeCacheMatrix returns. 

cacheSolve <- function(x, ...) {
        i <- x$getinvert()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinvert(i)
        i
}
