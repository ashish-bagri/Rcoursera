## A pair of functions are defined here which enable pre computation of certain values of a 
## data type. In this case, the data type is a matrix and the pre computed value is 
## the inverse of the matrix

# The function take as input a regular matrix object and converts to a special matrix
# the special matrix has the ability to cache it inverse
# if the inverse has been calculated previously, 
# it does not re-calculates it but simply returns the pre calculated value.
# If not pre-caluclated or updated, it calculates the inverse.

makeCacheMatrix <- function(x = matrix()) {
    # set the initial inverse to NULL
    inv <- NULL
    # set function to initialize the value
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # get function to return the underlying matrix
    get <- function() x
    
    # set a already computed inverse
    setinv <- function(inverse) inv <<- inverse
    
    # get the inverse value (either NULL or already computed value)
    getinv <- function() inv
    
    # return all the possible methods for this function
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This method calculates the inverse of a square invertible matrix
## It uses the solve function internally 
## The input to the function is a matrix created using the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # get the already cached inverse
    inv <- x$getinv()
    
    # if the cached inverse is not null, return that value
    if (!is.null(inv)) {
        message("Getting cached data")
        return (inv)
    }
    # comes here when cache value is NULL.
    # get the data (the underlying matrix)
    data <- x$get()
    # calculate the inverse using the solve function
    inv <- solve(data)
    # set the inverse in the cache
    x$setinv(inv)
    # return the newly calculated inverse
    inv
}
