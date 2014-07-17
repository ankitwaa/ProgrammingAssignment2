## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function function creates a "matrix" object that can cache its invers


makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
 	}
	get <- function() x
  	setinverse <- function(inv) inverse <<- inv
  	getinverse <- function() inverse
  	list(set = set, get = get,
       		setinverse = setinverse,
       		getinverse = getinverse )

}


## Write a short comment describing this function
## function checks for the inverse of the matrix in the cache, if the cache has 
## precomputed inverse of matrix it return the same inverse result otherwise com## putes it's inverse and return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
    		return(inverse)
  	}
  	data <- x$get()
  	inverse <- solve(data)
  	x$setinverse(inverse)
  	inverse
}
