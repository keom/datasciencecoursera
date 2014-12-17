## Function that is able to cache potentially time-consuming computations.
## This is to caching Inverse of a matrix rather than compute it repeatedly

## Functions are used to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
        
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## This is to returns the inverse of the matrix. It first checks if 
## The inverse has already been computed.If so, it gets the result and skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## To get solution 
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

##--------------------

## >  cacheSolve(m)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.066666

##--------------------

## from the cache in the second run

## > cacheSolve(m)
## getting cached data.
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667




