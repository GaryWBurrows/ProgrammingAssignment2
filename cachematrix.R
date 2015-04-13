#-----------------------------------------------------------------------------
# This file contains a set of functions that will invert a matrix and store
# the matrix and store its inverse in variables in manner that caches them
# so they may be retrieve from outside the function and without recalculating
# the inverse(provided the target matrix has not changed).
#----------------------------------------------------------------------------


#----------------------------------------------------------------------------
# This function defines a list of functions to perform the following
#
#  1. Cache the value of a matrix using data passed as an argument
#	The matrix is stored in manner that allows it to be cached
#
#  2. Returns the cached matrix or NULL if no matrix has been assigned
#
#  3. Cache a given matrix in the variable for the inverse matrix
#
#  4. Return the chached inverse matrix or NULL if it does not exist
#
#  This function returns a list containing the 4 fiuncitons defined in
#  this function block
#---------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {

	cached_inverse <- NULL   # Initialize inverse matrix NULL
	
	#+++++++++++++++++++++++++++++++++++++++++++++++++++
	# Store the value of the target matix using <<-
	# Set the inverse of the matrix to NULL using <<-
	#+++++++++++++++++++++++++++++++++++++++++++++++++++
	setmatrix <- function(a_matrix) {
		x <<- a_matrix
		cached_inverse <<- NULL
	}

	#++++++++++++++++++++++++++++++++++++++++++++++++++++
	# Return the cached matrix 
	#++++++++++++++++++++++++++++++++++++++++++++++++++++
	getmatrix <- function() {
		x
	}
	
	#++++++++++++++++++++++++++++++++++++++++++++++++++++
	# Store the matrix defined by i_matrix in cached_inverse 
	#+++++++++++++++++++++++++++++++++++++++++++++++++++
	setinverse <- function(i_matrix) {
		cached_inverse <<- i_matrix
	}

	#+++++++++++++++++++++++++++++++++++++++++++++++++++
	# Return the inverse matrix stored in cached_inverse
	#+++++++++++++++++++++++++++++++++++++++++++++++++++
	getinverse <- function() {
		cached_inverse
	}

	#++++++++++++++++++++++++++++++++++++++++++++++++++
	# return the list of funciton defining the function
	# makeCacheMatrix
	#++++++++++++++++++++++++++++++++++++++++++++++++++
	list (setmatrix = setmatrix, getmatrix = getmatrix,
		setinverse = setinverse, getinverse = getinverse)
}	


#----------------------------------------------------------------------------
# This function returns the inverse of a matrix (x). If the matrix 
# and its inverse have been previously stored then this function returns the
# inverse without recalculating it. If the matrix has been changed this
# function first recalculated the inverse and stores it in the special 
# retrievalble memory (cached_matrix) and returns it
#----------------------------------------------------------------------------
cacheSolve <- function(x, ...) {

	#+++++++++++++++++++++++++++++++++++++++++++++++++++
	# Retrieve the inverse matrix that is stored in the
	# makeCacheMatrix function 
	#+++++++++++++++++++++++++++++++++++++++++++++++++++
	imatrix <- x$getinverse()
	
	#+++++++++++++++++++++++++++++++++++++++++++++++++++
	# If imatrix, above, is NOT NULL then, the inverse 
	# has been cached. Return the inverse no need to re-
	# calculate the inverse
	#+++++++++++++++++++++++++++++++++++++++++++++++++++
	if (!is.null(imatrix)) {
		message("Getting cached inverse matrix")
		return(imatrix)
	}

	#++++++++++++++++++++++++++++++++++++++++++++++++++
	# If cached_inverse = NULL then the matrix changed
	# get the new matrix, compute its inverse, cache it
	# and return the inverse
	#++++++++++++++++++++++++++++++++++++++++++++++++++
	matrix <- x$getmatrix()
	imatrix <- solve(matrix)
	x$setinverse(imatrix)
	imatrix
}


