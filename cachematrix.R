## Function that returns a list of functions that can cache
## the result of an inverse matrix operation


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  solvemat <- function(solve) m <<- solve
  getmat <- function() m
  list(set=set,get=get,solvemat=solvemat,getmat=getmat)
}


## Solve an inputted matrix or get a cached value

cacheSolve <- function(x, ...) {
  m <- x$getmat()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$solvemat(m)
  m
}
