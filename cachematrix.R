## Programming Assignment 2
## Michael Matthews
## 20 June 2014
## R Programming
##
## The functions below work together to cache and return the inverse
## of a matrix
##

## makeCacheMatrix
## This function will cache the inverse of a matrix, along with
## functions for getting and setting the cached data

makeCacheMatrix <- function(x = matrix()) {
	# create a variable to store the inverse, set it to NULL
	# because it hasn't been calculated yet
	inv <- NULL
	
	# setMatrix will change the current matrix we are looking at
	# and reset the inverse variable
	setMatrix <- function( newX ) {
		x <<- newX
		inv <<- NULL
	}
	
	# gets the current matrix
	getMatrix <- function() x
	
	# sets the inverse matrix
	setInverse <- function( newInv ) inv <<- newInv
	
	# get the inverse of the current matrix
	getInverse <- function() inv
	
	# create a list of the above functions and return it
	list( setMatrix = setMatrix, getMatrix = getMatrix,
	      setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve
## This function will solve for the inverse of a matrix using
## the caching function defined above

cacheSolve <- function(x, ...) {
	# get the cached inverse matrix of x
	inverted <- x$getInverse()
	
	if (!is.null(inverted)) {
		# if the cached inversion is not null, it has already
		# been calculated so we can just return it
		message("Getting cached matrix...")
		return(inverted)
	}
	
	# otherwise, the inverse matrix needs to be calculated
	# and cached
	data <- x$getMatrix()
	inverted <- solve(data, ...)
	x$setInverse(inverted)
	
	# then we can return the inverse matrix
	inverted
}
