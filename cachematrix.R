makeCacheMatrix <- function(x = matrix()) {  #special matrix is created
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}




cacheSolve <- function(x, ...) {
m <- x$getinv()         #query the x vector's cache
  if(!is.null(m)) {     #if there is a cache    
    message("getting cached data")
    return(m)           #just return the cache, no computation needed
  }
  data <- x$get()         #if there's no cache
  m <- solve(data, ...)   #computation of inverse  
  x$setinv(m)              #save the result back to x's cache
  m       
        
}
