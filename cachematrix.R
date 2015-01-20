##Jose A Delgado. This  commits includes implementation for assignment 2.
## R Programming Data Science JH/Coursera course.
## This function creates a matrix type for caching ots inverse.
## accepts a squared matrix only
## get the value of  the matrix
## setInverse the inverse of the matix
## getInverse the inverse of the matix

## macheCacheMatrix. Sets, gets matrix. Sets, gets inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
 
  
  if(missing(x))
    stop("requires a matrix parameter...")
  i<-NULL


  set <- function(y){
 
    if(!is.matrix(y)) 
      stop("Not a matix...")
      
    if (nrow(y) != ncol(y))
      stop("Not a square matix...")
    x<<-y
    i<<-NULL
  }
  
  get<-function() x 
  setInverse <- function(inverse) i <<-inverse
  getInverse <- function() i
  getenv<- function() enviroment()
  
  mcm<-list(set = set, 
            get = get, 
            setInverse = setInverse, 
            getInverse = getInverse)
  set(x)

  class(mcm) <-"makeCacheMatrix"
  return(mcm)

}
print.makeCacheMatrix <- function(x){
  if(class(x)!="makeCacheMatrix") stop("Invalid type object...");
   x$get()
}

## If found, retrives an matrix inverse from the cache.
## Otherwise calculates it and save into the cache.

cacheSolve <- function(x, ...) {
 # print(x)
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached matrix")
    return(i)
}
 data <-x$get() 
 i <- solve( data )
 x$setInverse(i)
 i
}

##Test cases
m1<-makeCacheMatrix( matrix(c(3,2,-1,2,-2,4,-1,0.5,-1), c(3,3)))
##Caching & retrieveing
cacheSolve( m1 )
cacheSolve( m1 )

m2<-makeCacheMatrix( matrix(c(3,2,2,-2), c(2,2)))
##Caching & retrieveing
cacheSolve( m2 )
cacheSolve( m2 )

## Not square matrix test
m2<-makeCacheMatrix( matrix(c(3,2,2,-2,4,4), c(3,2)))
## Not matrix object parameter
m2<-makeCacheMatrix( list(c(3,2,2,-2,4,4), c(3,2)))

## Missed parameter
m2<-makeCacheMatrix( )
