##******************************************##
## file:    cachematrix.R                   ##
##     Caching the Inverse of a Matrix      ##


## This function creates a special "matrix" object that can cache its inverse.
## make a function list for cacheSolve to Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y) {
        ## reset the variables
        x <<- y
        iv <<- NULL
    }
    get <- function() {
        ## return the matrix data 'x'
        x
    }
    setinverse <- function(inverse) {
        iv <<- inverse ## Setting global variable iv
    }
    getinverse <- function() {
        iv             ## return global variable iv
    }
    
    ##return function list, for cacheSolve
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    iv <- x$getinverse()
    ## if iv is NULL or it's not matrix, need to caclulate it;
    ## otherwise, will return the inverse matrix of 'x' from cache
    if(!is.null(iv) && is.matrix(iv)) {
        message("getting caches data")
        return(iv)
    }
    data <- x$get()
    iv <- solve(data, ...)
    x$setinverse(iv) ## set inverse matrix for cache
    iv
}


## The following code is test demo

x  <- matrix(c(6,4,9,3,2,4,4,3,6),3,3)
iv <- makeCacheMatrix(x)
for(i in 1:3) {
    ## you can see three times inverse matrix of 'x'
    ## first is solve result, second & third is cache data for it 
    ## and two messages "getting caches data"
    print(cacheSolve(iv))
}
