## makeCacheMatrix and cacheSolve are 2 functions used together for caching the inverse of a matrix 
## Usage:
## > m1 <- matrix(c(1,2,3,4), nrow = 2)
## > m1cache <- makeCacheMatrix(m1)
## > m1Inv <- cacheSolve(m1cache)
## > m1invPrim <- cacheSolve(m1cache)
## getting cached data
## > m1 %*%  m1Inv
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1



## makeCacheMatrix receives as input a  square invertible matrix and returns a repository for 
## the same input matrix + its inverse matrix
## makeCacheMatrix doesn't compute itself the inverse 
## makeCacheMatrix returns in fact a list of accessors [get / set] for input matrix and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
				# add preconditions check
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverted <- function(inverted) inv <<- inverted
        getInverted <- function() inv
        list(set = set, get = get,
            setInverted = setInverted,
            getInverted = getInverted)
}


## cacheSolve is the 2nd function that receives as input the makeCacheMatrix output and outputs
## the cached inverse matrix
## if the inverse matrix is already computed and cached, an extra message is output as a proof 
## for using the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverted()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverted(inv)
        inv
}
