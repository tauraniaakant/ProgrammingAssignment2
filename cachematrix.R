## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse
## This function assumes that the input matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
        invertedMatrix <- NULL
        
        set <- function(y){
                x <<- y
                invertedMatrix <<- NULL
        }
        
        get <- function() x
        setInverse <- function(mat) invertedMatrix <<- mat
        getInverse <- function() invertedMatrix
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invertedMatrix <- x$getInverse()
        
        if(!is.null(invertedMatrix)){
                message("getting cached data")
                return(invertedMatrix)
        }
        
        invertedMatrix <- solve(x$get(), ...)
        x$setInverse(invertedMatrix)
        invertedMatrix
}
