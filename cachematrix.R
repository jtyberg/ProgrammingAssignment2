## This file contains two functions, `makeCacheMatrix` and `cacheSolve`.  
## Use the functions together to solve for the inverse of a matrix and cache the
## result.  This may be useful in situations when solving for the inverse of a 
## matrix is computationally expensive.

makeCacheMatrix <- function(x = matrix()) {
    # Function that caches a matrix and its inverse, and provides accessor 
    # functions to get and set the values of both
    #
    # Note: When setting the value of the matrix, the inverse is reset.
    #
    # Args:
    #   x: an invertible matrix
    # Returns:
    #   `list` object containing functions that allow access to the matrix and
    #   its inverse.  Use the list's `get` and `set` functions to access or 
    #   modify the matrix, and `get_inverse` and `set_inverse` functions to 
    #   access or set the inverse of the matrix.
    # 

    # Stores the inverse of the matrix in this function's environment.
    inv <- NULL
    
    # Accessor methods to retrieve the value of the matrix and its inverse
    get <- function() x
    get_inverse <- function() inv
    
    # Accessor methods to set the value of the matrix and its inverse.
    # We assign values to the matrix and its inverse using the `<<-` operator to 
    # enable searching the parent environment (this function) for the variable 
    # definitions.
    set <- function(y) {
        x <<- y
        # Reset the inverse (in case x != y)
        inv <<- NULL
    }
    set_inverse <- function(inverse) inv <<- inverse

    # Return a list of accessor methods for the matrix and its inverse
    list(set=set, 
         get=get,
         set_inverse=set_inverse,
         get_inverse=get_inverse)
}

cacheSolve <- function(x, ...) {
    # Returns the inverse of a matrix stored in 'x'.  If the inverse has not yet
    # been cached in 'x', this function will solve for the inverse and cache 
    # the result in 'x'; otherwise, it will simply return the cached value.
    #
    # Args:
    #   x: a `cache matrix` object that stores a matrix and its inverse, and
    #   has accessor functions to get and set the values of both.
    # Returns:
    #   The inverse of the specified cached matrix object.
    
    # If the inverse has already been cached, return the cached value.
    inv <- x$get_inverse()
    if (!is.null(inv)) {
        message('retrieving cached value of inverse')
        return (inv)
    }
    
    # Otherwise, solve for the inverse of the matrix and cache the result.
    message('solving for inverse')
    m <- x$get()
    inv <- solve(m, ...)
    x$set_inverse(inv)
    inv
}
