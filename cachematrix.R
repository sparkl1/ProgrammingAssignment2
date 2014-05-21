## Put comments here that give an overall description of what your
## functions do..................
## The "makeCacheMatrix()" function creates and initializes a "matrix" cache for subsequent use.
## The "cacheSolve()" function uses the cached "matrix" as input and updates the matrix inverse
##      value stored there (if it has been previously computed). 
##      If there is not previously computed inverse matrix, it computes one and updates the cache.

## The "set" pieces of these functions provide INPUT in to the cache.
## The "get" pieces of these functions get OUTPUT from the cache.

## TEST RESULTS ARE SHOWN AT END

## Write a short comment describing this function.................
## This function initializes a matrix value (caches it).
## It includes a place for the computed inverse matrix, initated to NULL.

makeCacheMatrix <- function(x = matrix()) {
    matinv <- NULL  ## Initialize the cached matrix inverse value as NULL 
    ## until it's calculated the first time.
    set <- function(y) {    ## The "set" variables provide the INPUT to the cache.
        x <<- y
        matinv <<- NULL     ## Sets the calculated matrix inverse to NULL 
        ## (i.e., not yet calculated)
    }
    get <- function() x     ## The "get" variables provide the OUTPUT from the cache.
    setmatinv <- function(matinvin) matinv <<- matinvin
    getmatinv <- function() matinv
    list(set = set, get = get,
         setmatinv = setmatinv,
         getmatinv = getmatinv)
}


## Write a short comment describing this function..................
## The following function computes the matrix inverse from a cached input matrix (which includes 
## a previously calculated value when the matrix inverse was already calculated). 
## If a new matrix inverse computation is performed, it is added to this cache. That is, the
## return value (the matrix inverse) from this function 
## (another matrix) is used as reference to subsequent computations, and is no longer the
## initialized NULL value.
## So long as this computed matrix inverse is not changed ("reinitialized"),
## the cached value of the matrix inverse is used (instead of being recomputed).


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matinv <- x$getmatinv()
    if(!is.null(matinv)) {
        message("getting cached data")
        return(matinv)
    }
    data <- x$get()
    matinv <- solve(data, ...)
    x$setmatinv(matinv)
    matinv
}
## TEST RESULTS FOLLOW ######################
## > testmatA
## [,1] [,2]
## [1,]    1    4
## [2,]    3    2
## > matAtst<-makeCacheMatrix(testmatA)
## > matAinvtst<-cacheSolve(matAtst)
## > matAinvtst
## [,1] [,2]
## [1,] -0.2  0.4
## [2,]  0.3 -0.1
## > matAinvtst<-cacheSolve(matAtst)
## getting cached data
## > matAinvtst
## [,1] [,2]
## [1,] -0.2  0.4
## [2,]  0.3 -0.1
## > matAinvtst %*% testmatA
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > ## TEST IS GOOD
