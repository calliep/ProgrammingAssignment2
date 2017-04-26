## To run, command line should read 'cacheSolve(makeCacheMatrix("data"))'
## The output should be equivalent to 'solve("data")'

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function()x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the "matrix" return by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        ## Return a matrix that is the inverse of 'x'
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        x$setinverse(inv)
        inv
}
