## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#the makeCacheMatrix function is a list of functions that set and get a matrix and the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
	
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	get <- function() x

	set_inverse <- function(inverse_input) inverse <<- inverse_input

	get_inverse <- function() inverse

	list(set = set,
		get = get,
		set_inverse = set_inverse,
		get_inverse = get_inverse)

}


## Write a short comment describing this function
#This function calculate de inverse of the matrix created by the makeCacheMatrix.
#This function check if the inverse matrix already exists, if yes, this just get the matrix, 
#skip the calculations and just get the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inverse <- x$get_inverse()
    	if(!is.null(inverse)) {
        	message("getting cached inverse")
        	return(inverse)
    	}
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    inverse
}
