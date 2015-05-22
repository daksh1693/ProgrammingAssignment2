## PROGRAMMING ASSIGNMENT 2

## makeCacheMatrix() creates a list of functions namely set(), get(), setinverse(), 
## getinverse(). This function is used to save the cached value 
## of the inverse of the matrix provided as input through the set() function.
## 
## cacheSolve takes a list as input. The list returned by the makeCacheMatrix can be used
## as input for this function. It first checks if the inverse has been calculated for the
## input matrix previously or not. If the inverse has been calculated previously, it uses
## the cached value saved, otherwise, it makes the calculation afresh and returns the 
## inverse.


## This function cretes a list of functions... cached inverse is also stored 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv<<-inverse 
        getinverse <- function() inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## This function returns the inverse of the input matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
