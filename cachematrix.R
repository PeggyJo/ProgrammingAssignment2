##
## Accepts a matrix and returns a list of functions to cache the matrix and its inverse
##
##   Sample code:
##      mm <- makeCacheMatrix( matrix( c(1:4), nrow=2, ncol=2) )
##      sourceMatrix <- mm.get()
##      invertedMatrix <- mm.getmatrix()  
##      mm.set(sourcematrix)
##      mm.setmatrix(invertedmatrix)
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        ## Returns the source matrix
        get <- function() {
                return(x)
        }
        
        ## Returns the inverted matrix
        getmatrix <- function() {
                return(m)
        }
        
        ## Loads a new source matrix and wipes out the inverted matrix
        set <- function (y) {
                x <<- y
                m <<- null
        }
        
        ## Returns the inverted matrix
        setmatrix <- function(imatrix) {
                m <<- imatrix
        }
        
        
        list(get=get, getmatrix=getmatrix, set=set, setmatrix=setmatrix)
}


## Returns a matrix that i the inverse of special matrix 'x'
## Note: x is created by passing a new matrix to function makeCacheMatrix
##
##    sample code:
##       mm <- matrix(rnorm(9), nrow=3, ncol=3)
##       theMatrix <- makeCacheMatrix(mm)
##       invMatrix  <- cacheSolve(theMatrix)
##
##       theMatrix.get()        ## displays the original matrix
##       theMatrix.getmatrix()  ## displays the inverted matrix
##
cacheSolve <- function(x=matrix(),  ...) {
        # Extracts the inverted matrix from the makeCacheMatrix object      
        m <- x$getmatrix()
        
        # If the cached inverted matrix is not null, return it instead of recalculating
        if ( !is.null(m) ) {
                message("Cached data available")
                return(m)
        }
        
        # Extracts the source matrix from the makeCacheMatrix object
        newMatrix <- x$get()
        
        # Inverts the source matrix
        m <- solve(newMatrix, ...)
        
        # Sets the inverted matrix in the makeCacheMatrix object
        x$setmatrix(m)
        
        # Return the inverted matrix
        m
}
