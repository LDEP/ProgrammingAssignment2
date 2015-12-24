## LDEP: R programming assignment 2; December 2015
##  	 Collection of functions that will
##	 1. makeCacheMatrix(): 
##		Create a "matrix" object X that can cache its inverse. 
##		It will be a list of methods: X=(set, get, setinverse, getinverse)
##		Usage Example:
##		A<-matrix(c(2,2,3,4,5,6,7,8,10),nrow=3,ncol=3) # Create matrix A, a 3x3 matrix 
##						# (nicely invertible)
##		X<-makeCacheMatrix() 		# Create matrix object X that can cache its inverse
##		X$set(A) 			# Essentially X = A, but X is a list of methods.
##
##	2. cacheSolve(X):
##		Invert the special matrix object X.
##		If the inverse of X has not yet been computed, find it with solve(X)
##		If the inverse of X is already in cache, it will be retrieved, and no new solve is done.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(X = matrix()) {  #Returns a list of functions.
	I <- NULL # Inverse I has no value yet.	
	set <- function(A){ 	#Assign values in matrix A to X
		X <<- A
		I <<- NULL
	}
	get <- function() X 	#Return what is in X
	setinverse <- function(inverse) I <<- inverse 	# I holds the inverse of X. 
							# "inverse" is just a parameter name.
	getinverse <- function() I			# Return inverse I
	
	# Return a list of methods
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'
	I <- X$getinverse()  	# Assign the inverse of X to I (if it exists)
	if(!is.null(I)) {	# If inverse I has already been computed, retrieve it from cache 	
		message("getting cached data")
		return(I)	# ...and exit the function.
	}
	matrixdata <- X$get() 	# Extract the matrix values from X
	I <- solve(matrixdata, ...)	# Solve for the inverse of X
	X$setinverse(I)		# Save the inverse of X using the setinverse method.
	I			# Return I, the inverse of X.
}
