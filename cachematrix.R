# The assignment is to write a pair of functions that 
# cache the inverse of a matrix


# A function to set/get the value of x, or set/get the
# inverse of the value x.
# -Variables: (the_matrix, inverse, matrix_to_set, inverse_to_set).
# -Functions: (set, get, set_inverse, get_inverse).
makeCacheMatrix <- function(the_matrix = matrix()) {
    
    # This is the variable which we will assign to it the inverse.
    inverse <- NULL
    
    # The 4 functions (set, get, set_inverse, get_inverse)
    set <- function(matrix_to_set) {
        the_matrix <<- matrix_to_set
        inverse <<- NULL
    }
    get <- function() {
        the_matrix
    }
    set_inverse <- function(inverse_to_set) {
        inverse <<- inverse_to_set
    }
    get_inverse <- function() {
        inverse
    }
    
    # Return a list containing the 4 functions
    list(set = set, get = get, set_inverse = set_inverse,
         get_inverse = get_inverse)
}


# Return the inverse of the matrix given in the arguments
# -Variables: (m_fun, inverse, the_matrix).
cacheSolve <- function(m_fun, ...) {
    
    # Get the inverse, if NULL continue otherwise return the value
    inverse <- m_fun$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # Get the matrix itself
    the_matrix <- m_fun$get()
    
    # Store the inverse of the matrix in a variable
    inverse <- solve(the_matrix, ...)
    
    # Set the inverse variable of m_fun to the computed value
    m_fun$set_inverse(inverse)
    
    # Return the inverse value
    inverse
    
}
