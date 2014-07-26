## I am using the given makeVector as a frame to create the makeCacheMatrix 
## function and cacheMean as frame for cacheSolve

## This is my implementation of makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## Here is my implementation of cacheSolve function
## it returns a matrix that is the inverse of 'x'
cacheSolve <- function(x) {
        
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix)
    x$setinverse(m)
    m
  
}
