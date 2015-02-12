## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# --------------------------
# This function creates a special 'matrix' that can cache its inverse.
# It was design to be used in conjuction with 'cacheSolve' function, just
# below.

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL # no matrix inverse at this time

    set <- function(mtx) {
        x <<- mtx # set a new matrix
        x_inv <<- NULL # reset the matrix inverse, need to be calculated again
    }
    get <- function() x # just return the matrix

    set_inverse <- function(mtx) x_inv <<- mtx # cache the matrix inverse
    get_inverse <- function() x_inv # return the cached matrix inverse

    # A list with all obj properties/functions.
    # I've included a 'isCachedMatrix' property, so calling functions can
    # determine what type of obj is this.
    list( set = set, get = get, get_inverse = get_inverse,
          set_inverse = set_inverse, isCachedMatrix = TRUE )
}

# -----------------------
# This function calculates the inverse of a matrix created using
# 'makeCacheMatrix' function. If an inverse is cached then it is returned.

cacheSolve <- function(x, ...) {
    # First check if 'x' is capable of caching a matrix inverse
    if( is.list(x) && isTRUE(x$isCachedMatrix) ) {

        # Try to find a cached value
        x_inv = x$get_inverse()
        if( !is.null(x_inv) ) {
            # Found! So return the cached value
            message("getting cached data")
            return(x_inv)
        }

        # No cache found, so we need to calculate the inverse
        x_inv = solve(x$get(), ...)
        x$set_inverse(x_inv) # cache the matrix inverse for future use
        x_inv # return the calculated one
    }
    else {
        message("Sorry, the matrix given was not created using makeCacheMatrix")
    }
}
