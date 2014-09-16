## Put comments here that give an overall description of what your
## functions do

## Creates an object with functions to get/set values of a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    I <- NULL                                   # initializes inverse to null
    
    set <- function(y) { x <<- y; I <<- NULL}       # define set function
    get <- function() x                             # define get function
    
    setinverse <- function(inverse) I <<- inverse   # sets inverse in cache
    getinverse <- function() I                      # gets inverse from cache
    
    # returns list of functions for get/set inverse values
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns inverse of matrix x. Retrieves cached value if already computed, 
## otherwise computes inverse and caches solution
cacheSolve <- function(x, ...) {    
    I <- x$getinverse()                     # gets cached value
    
    if(!is.null(I)) {
        message("getting cached data")      # reports getting cached value
        return(I)                           # returns inverse matrix I
    }
    
    data <- x$get()             # gets value of matrix x
    I    <- solve(data, ...)    # computes inverse of x, saves as matrix I
    x$setinverse(I)             # caches I
    I                   # returns inverse matrix I
}
