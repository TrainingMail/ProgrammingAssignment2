## Script contains two functions namely makeCacheMatrix that compute inverse of
## a matrix and and cacheSolve that caches it.

## makeCacheMatrix accepts input matrix and caches its inverse form

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL  			                                ## set variable i to null
  set <- function(y) {			                      ## set is defined here
    x <<- y
    i <<- NULL
  }
  
  get <- function() x			                        ## get is defined here
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,	...=                ## return list
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## cacheSolve accepts a square matrix created by makeCacheMatrix and checks if
## its inverse is cached. If cached, returns the inversed cache else 
## if not, computes its inverse and caches it and returns inverse matrix

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()  		                        ## assign the inverse of the input 
  if(!is.null(i)) {			                          ## check if the value of i exists in memory
    message("getting cached data")                ## return message that value is from memory
    return(i)                                     ## return value from memory and exit function
  }
  
  i <- solve(x$get())		                          ## else calculate using solve	 
  x$setinverse(i)                                 ## return the inverse
  i
  
}
