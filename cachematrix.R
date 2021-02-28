## Programming Assignment 2: Lexical Scoping
## Caching the Inverse of a Matrix

## makeCacheMatrix is a function factory to define matrix cache functions
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL       
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function() inv <<- solve(x)
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve save inverse matrix computatation to save resources
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverted matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv()
        inv
}