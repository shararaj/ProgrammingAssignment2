## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                            
  set <- function(y) {                   
    x <<- y                             ## value of matrix in parent environment
    inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x                     ## define the get function - returns value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## need this in order to refer 
  ## to the functions with the $ operator
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
  
}
