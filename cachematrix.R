## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Initializes the cahce and sets up a way to call it

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <-function() x
  setinverse <-function(inverse) inv <<- inverse
  getinverse <-function() inv
  list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)
  
}


## Write a short comment describing this function
## Runs a version of solve that checks if a user made cache exists and skips compution if it does

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
            message("getting cached data")
            return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
