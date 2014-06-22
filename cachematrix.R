## This assignment focuses on caching the inverse of a matrix.

## makeCacheMatrix will create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 m<- NULL
 set<- function(y){
   x<<-y
   m<<- NULL
 }
 get <- function() x
 setinverse<- function(solve) m<<- solve
 getinverse <- function() m
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve will compute the inverse of the special matrix that is returned by makeCacheMatix. If the inverse has already been calculated and the matrix has not changed then the cacheSolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  m<- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m ## Return a matrix that is the inverse of 'x'
}
