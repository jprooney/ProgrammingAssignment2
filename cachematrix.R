## This combination of functions takes a matrix as input and inverts
## that matrix. It caches the inverted matrix, and returns the inverted
## matrix from cache, as opposed to recalculating it, if it has not
## changed.

## makeCacheMatrix takes a matrix as input, and creates a list of 
## functions that are called in cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setMatrix <- function(y){
                x <<- y
                inverse <<- NULL
                ## setMatrix takes user input of new matrix, caches
                ## 'x' so it is availablle in makeCacheMatrix, and
                ## resets 'inverse' to NULL.
        }
        getMatrix <- function() x ## returns the input matrix 'x' 
        setInverse <- function(z) inverse <<- z
        ## takes the inverted matrix from cacheSolve, and caches it
        getInverse <- function() inverse
        ## returns the value of 'inverse', either NULL or an invertred 
        ## matrix
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
        ## creates a list of the functions defined above and references
        ## them with their names.
}


## cacheSolve checks for a cached value for the inverse of the matrix
## and returns it if it is there. Otherwise, it inverts the matrix,
## caches the inverse, and returns the inverted matrix.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
                ## sets the value of 'inverse' to that from makeCacheMatrix
        matr <- x$getMatrix()
                ## sets 'matr' to value of the matrix from makeCacheMatrix
        if (!is.null(inverse)){ ## checks if 'inverse' is NULL, if no...
                message("Getting cahced value...")
                return(inverse) ## returns the cached value of 'inverse'
        }
        ## if 'inverse' is NULL
        inverse <- solve(matr) ## inverts matr
        x$setInverse(inverse) ## sets 'inverse' in makeCacheMatrix 
        inverse ## returns inverted matrix
}
