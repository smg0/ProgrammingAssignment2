## makeCacheMatrix() creates a special CacheMatrix object from an
## invertible matrix. CacheMatrix object will hold a cache of its
## inverse matrix when calculated with cacheSolve. Modification to
## the CacheMatrix matrix data will clear any previously stored
## inverse cache value.
##
## cacheSolve() calculates the inverse of a CacheMatrix instance and
## updates the instance inverse cache. Future cacheSolve calls using
## the same CacheMatrix object will return the cached inverse matrix.

## Creates a CacheMatrix object which will store its inverse matrix
## once solved using cacheSolve
makeCacheMatrix <- function(x = matrix())
{
    ## store cached inverse matrix
    inverseMatrix <- NULL
    
    ## sets the matrix
    set <- function(y)
    {
        x <<- y
        
        # clear cached inverse on matrix update
        inverseMatrix <<- NULL
    }
    
    ## gets the matrix
    get <- function()
    {
        x
    }
    
    ## sets the cached inverse matrix
    setInverse <- function(i)
    {
        inverseMatrix <<- i
    }
    
    ## gets the cached inverse matrix
    getInverse <- function()
    {
        inverseMatrix
    }
    
    ## return named list containing methods of the CacheMatrix object
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Calculates the inverse of a CacheMatrix object and updates the
## object's cache
cacheSolve <- function(x, ...)
{
    ## get cached inverse matrix
    inverseMatrix <- x$getInverse()
    
    ## test if inverse has been previously set
    if(!is.null(inverseMatrix))
    {
        message("getting cached inverse matrix")
        
        ## return a matrix that is the inverse of 'x'
        return(inverseMatrix)
    }
    
    ## need to calculate inverse matrix
    
    ## get matrix
    matrix <- x$get()
    
    ## calculate inverse
    inverseMatrix <- solve(matrix)
    
    ## set the inverse matrix cache
    x$setInverse(inverseMatrix)
    
    ## return a matrix that is the inverse of 'x'
    inverseMatrix
}
