## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse  of a matrix rather than compute it repeatedly.

##We will write two functions that will be used to cache the inverse of a matrix.

##The first function 'makeCacheMatrix' will do the following:
## a) sets the value of the matrix which is to be inverted
## b) gets the value of the matrix
## c) sets the inverse 
## d) gets the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

##The second function 'cacheSolve' returns the inverse of the matrix. The 'if'
##loop check whether the inverse has been caluclated previously. If yes, returns
##the inverse and skips the calculation, if no than calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if(!is.null(inv)){
                message("Getting cached data!")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv) 
        inv
}
