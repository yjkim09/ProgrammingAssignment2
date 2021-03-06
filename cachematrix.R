##  In Programming Assignment 2, We use the lexical scoping to 
##  cache the matrix inverse if it is already calculated and is being
##  used second time.

## makeCacheMatrix defines two getter and two setter functions using
## the concept of lexical scoping
## four functions are returned as a list

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

## cacheSolve checks if inv exists, if so it pulls from the cache 
## in the memory, if not it calculates and set the inverse in the 
## memory
## return value is the inverse matrix

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
