## The functions store the Matrix and its Inverse into the Cache, which can be
## later retrieved as requried to avoid re- calculation

## makeCacheMatrix is a function which takes the matrix as an input
## and initializes the Cache Matrix with the provided matrix, 
## and its Inverse to NULL.
## If the matrix is not provided, it is initalized to an empty one.
## to create, and return the list of functions 
## which can handle setting the matrix data, retrieving it, also the same for inverse Of Matrix


makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL

  # Setter used to set the Matrix into the cache, so it can be checked to retrieve
  # the Inverse from Cache.
  set <- function(y){
    x <<- y
    inverseM <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inverseM <<- inverse
  
  getInverse <- function() inverseM
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve allows to retrieve the Inverse from Cache, if it has been 
## calculated once, and if not, it will do the calculation and store it 
## into Cache for future.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }

  mat <- x$get()

  inverseMatrix <- solve(mat, ...)
  
  x$setInverse(inverseMatrix)
  
  inverseMatrix
}
