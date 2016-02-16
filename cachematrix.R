## The objective of the functions outlined below is to allow for the caching and
## retrieval of the inverse of a specified matrix

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        set <- function(y){
                x <<- y
                i <<- NULL
        }
                get <- function() x
                setInverse <- function(solve) i<<- solve
                getInverse <- function() i
                
                list(set = set, 
                     get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)

     
}


## This function computes the inverse of a matrix. If the result has already been calculated,
## the cache result is returned, otherwise the inverse is calculated and then cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i = x$getInverse()
        
        if(!is.null(i))
        {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
