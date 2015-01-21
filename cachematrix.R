## Coursera - R Programming - Programming Assignment #2
## This R script includes two functions: makeCacheMatrix and cacheSolve
## These functions are used to calculate the inverse of a matrix allowing
## for caching of matrix inversion results when the function is called 
## multiple times.  This saves time for long running calculations for 
## which the input parameters haven't changed.
##
## ===============
## makeCacheMatrix
## ===============
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## ===============
## cacheSolve
## ===============
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## ===============
## Sample Run
## ===============
## Two matricies will be used to test: matA and matB
## matA <- matrix(c(1,0,5,2,1,6,3,4,0), 3, 3)
## Expected Inverse of matA: [(-24,20,-5),(18,-15,4),(5,-4,1)]
##
## matB <- matrix(c(3,2,0,0,0,1,2,-2,1), 3, 3)
## Expected Inverse of matB: [(0.2, -0.2, 0.2),(0.2,0.3,-0.3),(0,1,0)]
##
## 1. Calc inverse of A.  Expected: Results from solve function
## ------------------------------------------------------------
## > matAinv <- makeCacheMatrix(matA)
## > cacheSolve(matAinv)
## [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
##
## 2. Calc inverse of A.  Expected: Results from cached function
## -------------------------------------------------------------
## > cacheSolve(matAinv)
## getting cached data
## [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
##
## 3. Calc inverse of B.  Expected: Results from solve function
## ------------------------------------------------------------
## > matBinv <- makeCacheMatrix(matB)
## > cacheSolve(matBinv)
## [,1] [,2] [,3]
## [1,]  0.2  0.2    0
## [2,] -0.2  0.3    1
## [3,]  0.2 -0.3    0
##
## 4. Calc inverse of B.  Expected: Results from cached function
## ------------------------------------------------------------
## > cacheSolve(matBinv)
## getting cached data
## [,1] [,2] [,3]
## [1,]  0.2  0.2    0
## [2,] -0.2  0.3    1
## [3,]  0.2 -0.3    0
##
## 5. Calc inverse of A.  Expected: Results from cached function FOR A (to 
##                                  verify function didn't confuse A and B)
## ------------------------------------------------------------------------
## > cacheSolve(matAinv)
## getting cached data
## [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## >