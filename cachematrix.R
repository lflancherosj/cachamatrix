###C reates a matrix that allows cache its inverse####

makeCacheMatrix <- function(x = matrix()) {
  J <- NULL
  set <- function(y){
    x <<- y
    J <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Regresar una matriz inversa a 'x' ##
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
