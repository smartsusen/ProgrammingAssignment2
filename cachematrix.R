##Jose A Delgado. This  commits includes implementation for assignment 2.
## R Programming Data Science JH/Coursera course.
## This function creates a matrix type for caching ots inverse.
## accepts a squared matrix only
## get the value of  the matrix
## setInverse the inverse of the matix
## getInverse the inverse of the matix

## macheCacheMatrix. Sets, gets matrix. Sets, gets inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  i<-NULL
  
  mcm<-list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
  
  if(missing(x))
    stop("requires a matrix parameter...")
  
  if(!is.matrix(x)) 
    stop("Not a matix")

  if (nrow(x) != ncol(x))
    stop("Not a square matix...")

  
  set <- function(y){
    print(y)
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
  
 # mcm <- list2env(mcm)
  class(mcm) <-"makeCacheMatrix"
  return(mcm)

}
print.makeCacheMatrix <- function(x){
  if(class(x)!="makeCacheMatrix") stop("Invalid type object...");
  c( c("Matrix: ",x$get()))
}

## If found, retrives an matrix inverse from the cache.
## Otherwise calculates it and save into the cache.

cacheSolve <- function(x, ...) {
  i <- X$getInverse()
  if(!is.null(i)){}
    message("getting cached matrix")
}
 data <-x$get() 
 i <- solve(x)
 x$setInvernse(data)
 i
}
