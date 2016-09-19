## This file contains a pair of functions that cache the inverse of a matrix.

## 1) makeCacheMatrix creates a special "super-matrix" object that can cache
## the inverse invA of a matrix A.
## The function returns a list containing four functions to
## *set the value of the matrix
## *get the value of the matrix
## *set the value of the inverse (warning: manipulate with care)
## *get the value of the inverse

makeCacheMatrix <- function(A = matrix(1)) {
    invA <- NULL
    set <- function(B) {
        A <<- B
        invA <<- NULL
    }
    get <- function() A
    setinv <- function(inv) invA <<- inv
    getinv <- function() invA
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## 2) cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(A, ...) {
    invA <- A$getinv()
    if(!is.null(invA)) {
        message("getting cached inverse")
        return(invA)
    }
    matrix <- A$get()
    invA <- solve(matrix, ...)
    A$setinv(invA)
    invA
}
