## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. Wrote a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        ## initialize the variables
        Inv_m <- NULL
        
        ## initialize the set function
        set <- function(y) 
        {
              ## a search parent environments for an existing definition of the variable being assigned. 
              ## If such a variable is found then its value is redefined, 
              ## otherwise assignment takes place in the global environment
              x <<- y       
              Inv_m <<- NULL
        }
        
        get <- function() x  ## assigns value to get
        
        ## store the cached invers of the matrics
        set_inverse <- function(inver_mat) Inv_m <<- inver_mat
        ## retrives the value
        get_inverse <- function() Inv_m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
         ## retrive the previously stored value
         Inv_m <- x$get_inverse()
        
         ## check if the value exist
        if(!is.null(Inv_m)) 
        {
                
                message("getting cached data")
                
                ## if exit then return the value
                return(Inv_m)
        }
        ## if data doesnt not exit then retrive the input value
        data <- x$get()
        
        ## computes the inverse of the matric
        Inv_m <- solve(data)
        
        ## Cache the inverse 
        x$set_inverse(Inv_m)
        
        ## Return a matrix that is the inverse of 'x'
        Inv_m
        
}
