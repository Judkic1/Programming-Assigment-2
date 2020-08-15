## Matrix conversion typically requires costly, time-consuming calculations. 
## By caching the inverse efficiency is gained.

## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        matrix <- NULL
        set <- function(y){
                x <<- y
                matrix <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list(set = set,
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## This function computes the inverse of the "matrix" object. 
## If the inverse has already been calculated, then it will retrieve the data.

cacheSolve <- function(x, ...) {
        if (!is.null(inv)){
                message("Getting Cached Data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$set_inverse(inv)
        ## Return a matrix that is the inverse of 'x'
}