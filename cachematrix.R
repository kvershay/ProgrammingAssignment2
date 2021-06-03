## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function creates a matrix 
## it can also be called to retrieve the value of the matrix
## as well as set and retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) i <<- inverse
    getInv <- function() i
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## The cacheSolve function calculates the inverse, using the solve function,
## of the matrix created in the makeCacheMatrix function 
## before it calculates the inverse, it checks to see if it was calculated
## before and retrieves the calculation from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInv()
    if(!is.null(i)){
        message ("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInv(i)
    i
}
