## Put comments here that give an overall description of what your
## functions do
## these functions take a matrix, and cache it for future use
## usage:
## source('cachematrix.R')
## mat <- makeCacheMatrix(matrix(1:4,2,2))
## mat$getIn() 'it will return null for first time'
## cacheSolve(mat) 'caches the matrix and returns it'
## mat$getIn() 'returns inverse matrix'

## Write a short comment describing this function
## Accepts a matrix and stores it's Inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	#sets the accepted matrix
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	# returns the given matrix
	get <- function() x
	
	# sets inverse of given matrix
	setIn <- function(inverse)
	  { inv <<- inverse}
	
	#gets inverse of given matrix
	getIn <- function() inv
	
	#list of all functions
	list(set = set, get = get,
	     setIn = setIn, getIn = getIn)
}


## Write a short comment describing this function
## accept matrix and returns it's inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Gets inverse matrix if exists
  inv <- x$getIn()
  ## checks if inverse matrix exists, if it does, it returns it
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## gets original matrix
  m <- x$get()
  ## inverses the matrix
  inv = solve(m, ...)
  ## sets inverse matrix in cache
  x$setIn(inv)
  ## returns inverse matrix
  inv
}

