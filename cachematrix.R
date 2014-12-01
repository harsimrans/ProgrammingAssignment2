## helps to cache the inverse of matrix

## helps to cache a matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
	
	m <- NULL
	
	# set the matrix
	set <- function (y) {
		mat <<- y
		m <<- NULL
	}
	
	# get the matrix
	get <- function() { mat }
	
	# set the inverse of a matrix
	set_inverse <- function (inverse){
		m <<- inverse
	}
	
	# get the inverse of a matrix
	get_inverse <- function () { m }
	
	# list of methods to return
	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)

}


## Compute the inverse of the matrix if the result has not been cached
## otherwise look up the cache and pick the result from it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # get the matrix
        data <- x$get()
        
        # calculate the inverse
        m <- solve(data)
        
        # set the inverse
        x$set_inverse(m)
        
        # return matrix       
        m
}
