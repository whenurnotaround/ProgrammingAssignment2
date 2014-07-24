## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m.inverse <- NULL
    set <- function(y) {
        x <<- y
        m.inverse <<- NULL
    }
    get <- function() x
    
    set.inverse <- function(inverse) m.inverse <<- inverse
    get.inverse <- function() m.inverse
    
    list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m.inverse <- x$get.inverse()
    if(!is.null(m.inverse)) {
        message("getting cached data")
        return(m.inverse)
    }
    data <- x$get()
    m.inverse <- solve(data, ...)
    x$set.inverse(m.inverse)
    m.inverse
}
