## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
	# Return a a special "matrix" object as a list object that can cache its inverse
    inv <- NULL # initializing inverse as NULL
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv <<-inverse # caching inverse
    getinverse <- function() inv
    list(set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse) # list object that is returned
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) # Check whether the inv(inverse of the matrix) has already been calculated
    {
		# inv(inverse of the matrix) is already present in the cache
        message("inverse is already cached...getting cached data")
        return(inv)
    }
	# inv(inverse of the matrix) needs to be calculated
    mat <- x$get() # call the get() function to get the matrix whose inverse is to be calculated 
    inv <- solve(mat) # solve function returns the inverse of the matrix 'mat' in 'inv' variable
    x$setinverse(inv) # setting the inverse in the cache
    inv # return inverse of the matrix
}
