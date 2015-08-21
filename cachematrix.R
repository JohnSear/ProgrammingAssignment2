## These functions are used to implement a new matrix 'special vector' that support caching of it's inverse

## This function create a special 'vector' - which holds matrix data, it's cached inverse (if it has one) and functions get and set this data
makeCacheMatrix <- function( tmpMatrix = matrix() ) {
    ## Note - I've used tmpMatrix - so I can call the set function
	##        This guarantees the same error checking as calling set directly
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
		
		## additional testing to check for invalid matrix types
		if (is.null(x)) {
			message( "Matrix is null" )
		} else if ( nrow(x) == 1 & ncol(x) == 1 & is.na( mEmpty[1,1] ) ) {
			message( "Matrix is empty" )
		} else if ( nrow(x) != ncol(x) ) {
			message( "Inverse only works on square matrices" )
		}
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	set( tmpMatrix )
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## if needed this updates the cached matrix inverse in a cacheMatrix. If the cache is empty it creates a new matrix inverse and stores it.
cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
			message("getting cached data")
			return(i)
	}
	data <- x$get()
	
	i <- solve(data, ...)
	x$setinverse(i)
	i			
}

## Here are a few tests - I've commented these out completely so there aren't errors when this file is ran

## A 3x3 matrix - works fine
##m33 <- matrix( rnorm(9), nrow=3)
##cm33 <- makeCacheMatrix(m33)
##cm33$get()
##cm33$getinverse()
##cacheSolve(cm33)
##cm33$getinverse()
##cacheSolve(cm33)
##cm33$getinverse()

## A 3x2 matrix - should fail (but highlighted during matrix creation)
##m32 <- matrix( rnorm(6), nrow=2)
##cm32 <- makeCacheMatrix(m32)

## NULL case - should fail (but highlighted during matrix creation)
##cmNull <- makeCacheMatrix(NULL)

## An empty matrix - should fail (but highlighted during matrix creation)
##mEmpty <- matrix();
##cmEmpty <- makeCacheMatrix(mEmpty)




