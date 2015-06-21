## ASSIGMENT 2
## Alex Dawn @ 21/06/2015

##Functions are based on the example functions.
## makeCacheMatrix is given a standard matrix to produce a list object which holds the getters and setters for the matrix and its inverse
## cacheSolve is given a list object produced by makeCacheMatrix and returns a matrix which is the inverse.

## like the example code for calculating the mean of a vector, this produces a special matrix which is a list of four functions
## internal variables are x and i, which are the matrix and its inverse
## functions are get and set for both the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function first checks to see if x already contains an inverse matrix and if so returns it,
## otherwise it first gets the actual values of the matrix and runs the solve function on it
## before caching the inverse and returning it.
## function is passed the matrix x
## the internal variable m is used to store the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("Using prior cached matrix")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
