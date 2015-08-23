# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than compute it repeatedly
# The following functions allow to create a special "matrix" whose inverse
# is cached for later use.

# creates a special "matrix", which is really a list of setters and getters
# for the matrix itself and its inverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() {
		x
	}

	setInverse <- function(inverse) {
		i <<- inverse
	}

	getInverse <- function() {
		i
	}

	api <- list(set = set,
		    get = get,
		    setInverse = setInverse,
		    getInverse = getInverse)

	invisible(api)
}


# returns the inverse of the special "matrix" created with the above function;
# it first checks for a cached version
cacheSolve <- function(x, ...) {
	i <- x$getInverse()

	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)

	i
}
