## Program Begins


## To create special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initializing the inverse
  i <- NULL
  
  ## Setting the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Gettig the matrix
  get <- function() {
    m
  }
  
  ## Setting the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Getting the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
cacheSolve <- function(x, ...) {
  
  ## Returning the inverse matrix 
  m <- x$getInverse()
  
  ## Returning the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Execution comes to this part of the inverse is not already set
  data <- x$get()
  
  ## Calculating the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Setting the inverse 
  x$setInverse(m)
  
  m
}