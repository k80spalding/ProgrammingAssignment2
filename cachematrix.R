## A pair of functions that cache the inverse of a matrix.
## Assume that the matrix supplied is always invertible.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {    ##Pass a matrix as an argument
        inv <- NULL
        set <- function(y) {         
                x <<- y                ##Assigns y to an object in makeCacheMatrix
                inv <<- NULL           ##Clears any value of inv that has been cached by a prior execution of cacheSolve
        }
        get <- function() x      ##Defines the getter. As x is not defined in the get function, R retrieves it from makeCacheMatrix
        setinverse <- function(inverse) inv <<- inverse   ##Defines the setter. R assigns this to inv in makeCacheMatrix
        getinverse <- function() inv                  ##R retrieves inv from makeCacheMatrix
        list(set = set, get = get,                ##Output is a list of values to be calculated, named for use in cacheSolve
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## Returns a matrix that is the inverse of 'x'
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {    ##Argument passed in is the list output from makeCacheMatrix
        inv <- x$getinverse()           ##cacheSolve attempts to retrieve the inverse matrix from the object passed in as an argument by calling the getinverse function
        if(!is.null(inv)) {             ##Then checks to see if result is not null ie if there is already a cached inverse matrix
                message("getting cached data") ##If yes, it will give this message
                return(inv)                   ##And return the cached inverse matrix
        }
        data <- x$get()                 ##If there is no cached inverse matrix, cacheSolve gets the matrix from the input object
        inv <- solve(data, ...)         ##Then calculates the inverse
        x$setinverse(inv)               ##Sets the inverse matrix in the makeCacheMatrix output list
        inv                             ##Returns inverse matrix
}

