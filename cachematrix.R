## Created for: Elias Junior from Brazil
## Date: 21/03/2015
## This code fetch the inverse matrix using cache for better performance

## This function stores the value of my matrix in 
## a variable, and create methods to assign and take 
## the value of my inverse matrix.

makeCacheMatrix <- function(mymatrix = matrix()) {
			invm <- NULL
            setmymatrix <- function(y) {
                    invm <<- NULL
					mymatrix <<- y
            }
            getmymatrix <- function() mymatrix
            setmyInvmatrix <- function(solve) invm <<- solve
            getmyInvmatrix <- function() invm
            list(setmymatrix = setmymatrix, 
				 getmymatrix = getmymatrix,
                 setmyInvmatrix = setmyInvmatrix,
                 getmyInvmatrix = getmyInvmatrix)
}

## In this function I use the methods / functions previously 
## created to store the value in the cache and perform the 
## inversion of the matrix stored in my global variable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		invm <- x$getmyInvmatrix()
            if(!is.null(invm)) {
                    message("getting cached data")
                    return(invm)	
            }
            data <- x$getmymatrix()
            invm <- solve(data, ...)
            x$setmyInvmatrix(invm)
			invm
}
