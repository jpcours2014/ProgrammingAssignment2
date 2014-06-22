## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

 ## set the value of the matrix
  ix <- NULL
  set <- function(y) {
    x <<- y
    ix <<- NULL
 }
 ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse<- function(inv) ix <<-inv
  getinverse <- function() ix
  
  ## get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'

   ## get the inverse of the matrix
    ix <- x$getinverse()
    
    
    ## check if there is the matrix
    if (!is.null(ix)) {
         message("getting cached inverse matrix")
         return(ix)
         
    } else {
    ## if not: get the inverse of the matrix
      ix <- solve(x$get())
      
    ## set the inverse of the matrix 
      x$setinverse(ix)
      return(ix)
    }

}
