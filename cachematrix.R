#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(y = matrix()) {
  j <- NULL
  set <- function(p){
    y <<- p
    j <<- NULL
  }
  get <- function()y
  setinverse <- function(inverse) j <<- inverse
  getinverse <- function() j 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}
 
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(y, ...) {
  j <- y$getinverse()
  if(!is.null(j)){
      message("getting cached data")
    return(j)
  }
  mat <- y$get()
  j <- solve(mat,...)
  y$setInverse(j)
  j
}