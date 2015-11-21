## These functions store a matrix in a special matrix-like object.
## First the matrix-like object is created from an ordinary matrix and stored,
## e.g. cached, in a special list.
## When the inverse of that matrix needs to be computed (we're using the "solve()"
## function for this)  the function cacheSolve checks if the inverse has been 
## computed earlier. If there is a solution, the solution will be returned. Else
## the solution will be computed.

## three test matrices
## they aren't necessary for this assignment

A <- matrix(rnorm(6), nrow=3, ncol=2)   # not a square matrix
B <- matrix(1:9, nrow=3, ncol=3)        # not invertible
C <- matrix(rnorm(9,1), nrow=3, ncol=3) # most likely invertible

## makeCacheMatrix creates a special "matrix, which contains the inverse a matrix.
## first the function checks if the matrix is square and invertible
## then it caches the matrix in a new object

makeCacheMatrix <- function(x = matrix()) {
    if (dim(x)[1] != dim(x)[2]) {
        stop("The matrix is not a square matrix.")
    }
    if (det(x) == 0) {
        stop("The matrix is not invertible.")
    }
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## cacheSolve checks for an inverse matrix. if there is one, it will be returned.
## else cacheSolve computes the inverse matrix and caches it the "matrix"-like object
## created earlier with makeCacheMatrix.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
