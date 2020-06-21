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