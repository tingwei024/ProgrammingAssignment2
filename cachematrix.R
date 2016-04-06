## Caching the Inverse of a Matrix
## functions do
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) cache <<- inverse
    getInv <- function() cache
    list(set = set, get = get,setInv = setInv,getInv = getInv)
}


## The cacheSolve function calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cache <- x$getInv()
    if(!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }
    data <- x$get()
    cache <- solve(data, ...)
    x$setInv(cache)
    cache
}
