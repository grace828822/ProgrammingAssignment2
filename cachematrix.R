# The following are two function that cache and compute the inverse of matrix.

# This "makeCacheMatrix" create a matrix object which can cahce its matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL         ## Initializing the inverse property

	## Method to set the matrix
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }

    ## Method to get the matrix
    get <- function() return(mtx);

    ## Method to set the inverse of the matrix
    setinv <- function(inv) inverse <<- inv;  ## storing inverse

    ## Method to get the inverse of the matrix
    getinv <- function() return(inverse);

    ## Returns the list of methods
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))  ## returns the inverse
}


# After recieving the above function object, "cacheSolve" can computer the inverse of this matrix.
# If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {

    ## returns if the inverse has already been calculated (i.e. if !is.null(m)==TRUE)
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }

    data <- mtx$get()	## getting the matrix from our object
    invserse <- solve(data, ...)
    mtx$setinv(inverse) ## storing the inverse to the object to future usage
    return(inverse) 	## returning a matrix that is the inverse of 'x'
}
