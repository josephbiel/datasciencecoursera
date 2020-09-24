## makeCacheMatrix
##
## Accepts as input a matrix whose inverse is to be cached
## Returns a list that can be passed to the cacheSolve function
##
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve
##
## Accepts as input a list returned by the makeCacheMatrix function
## Returns the inverse of the matrix passed to the makeCacheMatrix function
##
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
