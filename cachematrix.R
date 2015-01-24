## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 Inv_m <- NULL
        set <- function(y) {
                x <<- y
                Inv_m <<- NULL
        }
        get <- function() x
        set_inverse <- function(inver_mat) Inv_m <<- inver_mat
        get_inverse <- function() Inv_m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         Inv_m <- x$get_inverse()
        if(!is.null(Inv_m)) {
                message("getting cached data")
                return(Inv_m)
        }
        data <- x$get()
        Inv_m <- solve(data)
        x$set_inverse(Inv_m)
        Inv_m
        
}
