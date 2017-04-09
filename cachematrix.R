## Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse

## This function creates a special "matrix" object that can cache its inverse, which is really is
## a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(Mat) inv <<- Mat
    getInv <- function() inv
    list(set = set, get  = get, setInv = setInv, getInv = getInv)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the matrix in the cache via the `setMatrix`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setInv(inv)
        inv
}
