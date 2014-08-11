## morowulf
## 08/11/2014
## R Programming
## Assignment 2


## Used together cacheSolve() and makeCacheMatrix() decrease the workload
## needed to compute an inverse matrix using the solve() function by creating
## and using a special matrix object which contains information about
## the original matrix as well as the computed inverse matrix.

## makeCacheMatrix creates an object which contains methods to store
## and retrieve the original input matrix or the corresponding inverse
## matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(invdata) m <<- invdata
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## cacheSolve takes a "matrix" object created by the makeCacheMatrix()
## function and then either computes the inverse of the input matrix
## or retrieves the cached version stored in the input object.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}
