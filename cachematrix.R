## The two functions contained within the cachematrix.R file allow 
## take advantage of lexical scoping within R to optimize on computation
## time. 

## makeCacheMatrix creates a list containing multiple special matrix
## object that allow for:
##      - storing of a matrix
##      - setting of a matrix
##      - caculation & caching of the inverse of the matrix
##      - retrieval of the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverseOfX) inv <<- inverseOfX
        getInv <- function() inv
        
        list(set = set, 
             get = get, 
             setInv = setInv, 
             getInv = getInv)
}


## The function cacheSolve calculates the inverse of the "matrix"
## object from makeCacheMatrix. If the inverse has already been
## calculated, it returns this value w/o additional calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)){
                message("Returning cached data!")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
        m
}
