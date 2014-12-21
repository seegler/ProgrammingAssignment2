## Functons for computing the inverse of a matrix and caching the result for
## subsequent retrievals. 
## Uses solve() to calculate the inverse
## Takes advantage of R scoping rules to cache the computed inverse
## Usage:
## 	> x <- matrix(c(1,3,2,4), nrow=2, ncol=2)
## 	> mx <- makeCacheMatrix(x)
## 	> cacheSolve(mx)
## 		##computes the inverse first time
## 	> cacheSolve(mx)
## 		##returns the inverse from cache without computing

## Creates a special "matrix", which is really a list containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse of the matrix
##    get the value of the inverse of the matrix
## cacheSolve() function takes this special matrix as input to compute and
## cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	
	## set the value of the matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## get the value of the matrix
	get <- function() x
	
	## set the value of the inverse of the matrix
	setsolve <- function(solve) m <<- solve
	
	## get the value of the inverse of the matrix
	getsolve <- function() m
	
	## return the list
	list(set = set, get = get,
	setsolve = setsolve,
	getsolve = getsolve)
}


## Given a makeCacheMatrix object, return the inverse of the matrix.
## Returns the cached inverse if it exists. If cached value doesn't exist,
## computes the inverse using solve() and caches the value in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Get the cached inverse
        m <- x$getsolve()
	
	## If the cached inverse is not null, return it
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	## cache is empty. Compute the invese and cache the result
	
	##Get the input matrix
	data <- x$get()
	
	##Compute the inverse
	m <- solve(data, ...)
	
	##Cache the inverse
	x$setsolve(m)
	
	##Return the inverse
	m
}
