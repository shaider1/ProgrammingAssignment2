## These functions essentially performed the matrix inverse( assuming the matrix is invertible).
## If the matrix inverse is previously calcuated, the following functions optimizes the execution
## by caching the inverse in the memory ( inv variable). This helps to reduce the time consuming computations.



## makeCacheMatrix is the main function that stores the list of four functions set, get, setinverse, getinverse.
## get is a function that returns the matrix stored in the main function
## set is a function that changes the matrix stored in the main function
## setinverse is a function that stores the inverse in the main function
## getinverse is a function that returns the inverse


makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## cacheslve firstly verify the value inv, stored previously with getinverse. 
##If the inverse exists in memory, it simply returns a message and the value inv, that is supposed to be the inverse.
## If the inverse NOT exists in memory, the function gets the matrix using get(),calculates the inverse using solve() instruction and
## sets the inverse using set() function.

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
