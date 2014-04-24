## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it repeatedly.
## functions makeCacheMatrix and cacheSolve are used to cache the inverse of a matrix


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse, 
## which is really a list containing a function to:
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        # store the matrix in an environment that is different from the 
        # current environment
        x <<- y      
        inv <<- NULL # reset the inverse to force its calculation
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse # store inverse in different environment
    getinv <- function() inv
    # return the set/get functions (for matrix and it's inverse) as a list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    #check if the inverse has already been calculated; if yes, return it.
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #at this point, the inverse has not been calculated for the current stored matrix
    data <- x$get() #get the stored matrix
    inv <- solve(data, ...) #calculate its inverse
    x$setinv(inv) #and cache the inverse for future use
    inv # return the inverse
}
