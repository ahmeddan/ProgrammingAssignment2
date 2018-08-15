## returns a list of the functions to set a matrix, get the matrix, set the inverse of a matrix and get the inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {

        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invrs <<- inverse
        getinverse <- function() invrs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Takes the list from the first function and computes the inverse of the matrix defined within it, if matrix is already set it will skip computation of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invrs <- x$getinverse()
    if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
    
	data <- x$get()
    
	invrs <- solve(data, ...)
    
	x$setinverse(invrs)
    
	invrs
}
