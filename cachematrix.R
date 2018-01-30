## The two following functions take a given matrix and first cache it and then calculate its inverse and 
## then cache that data

## The following function takes a given matrix and using four nested functions sets up the variables with all 
##of the needed data for the following function to then calculate the inverse

makeCacheMatrix <- function( x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
    
  }
  get <- function()x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function first calculates the inverse of the given matrix if there is no cache data already there, but returns
## the inverse of the matrix if it has already calculated it

cacheSolve <- function(x,...){
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
